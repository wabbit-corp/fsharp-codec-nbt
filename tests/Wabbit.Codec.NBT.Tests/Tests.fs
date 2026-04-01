module Wabbit.Codec.NBT.Tests

open System
open Wabbit.Codec.NBT
open Xunit

[<Fact>]
let ``named roundtrip preserves nested values`` () =
    let tag =
        TagOps.compound
            [ "name", Tag.String "Oak"
              "health", Tag.Short 12s
              "items",
              TagOps.list
                  TagType.Compound
                  [ TagOps.compound [ "id", Tag.String "stick"; "count", Tag.Byte 2y ]
                    TagOps.compound [ "id", Tag.String "stone"; "count", Tag.Byte 5y ] ]
              "bytes", Tag.ByteArray [| 0uy; 127uy; 255uy |]
              "numbers", Tag.IntArray [| 1; 2; 3 |] ]

    let bytes = Serialization.toByteArray "root" tag
    let decoded = Serialization.fromByteArray bytes

    Assert.Equal<Tag>(tag, decoded)

[<Fact>]
let ``raw little endian roundtrip is supported`` () =
    let tag =
        TagOps.compound
            [ "temperature", Tag.Float 18.5f
              "wind", Tag.Int 4
              "label", Tag.String "north" ]

    let bytes = Serialization.toRawByteArrayWith ByteOrder.LittleEndian tag
    let decoded = Serialization.fromRawByteArrayWith ByteOrder.LittleEndian bytes

    Assert.Equal<Tag>(tag, decoded)

[<Fact>]
let ``snbt escapes strings and signed bytes`` () =
    let tag =
        TagOps.compound
            [ "message", Tag.String "say \"hi\" \\ wave"
              "payload", Tag.ByteArray [| 0uy; 255uy |] ]

    let snbt = TagOps.toSnbt tag

    Assert.Equal("{message: \"say \\\"hi\\\" \\\\ wave\", payload: [B;0B, -1B]}", snbt)

[<Fact>]
let ``long array roundtrip is supported`` () =
    let tag =
        TagOps.compound
            [ "palette", Tag.LongArray [| 1L; -2L; 3L |]
              "label", Tag.String "packed" ]

    let bytes = Serialization.toByteArray "root" tag
    let decoded = Serialization.fromByteArray bytes

    Assert.Equal<Tag>(tag, decoded)

[<Fact>]
let ``zlib compressed named roundtrip is supported`` () =
    let tag =
        TagOps.compound
            [ "name", Tag.String "zlib"
              "values", Tag.IntArray [| 7; 8; 9 |] ]

    let bytes = Serialization.toCompressedByteArray Compression.ZLib "root" tag
    let decoded = Serialization.fromByteArrayAuto bytes

    Assert.Equal<Tag>(tag, decoded)

[<Fact>]
let ``reader policy can limit nesting depth`` () =
    let tag =
        TagOps.compound
            [ "outer",
              TagOps.compound
                  [ "inner",
                    TagOps.compound [ "value", Tag.Int 1 ] ] ]

    let bytes = Serialization.toByteArray "root" tag

    Assert.ThrowsAny<System.Exception>(fun () ->
        Serialization.fromByteArrayWithAndPolicy
            ByteOrder.BigEndian
            { ReaderPolicy.Default with
                MaxDepth = Some 1 }
            bytes
        |> ignore)
    |> ignore

[<Fact>]
let ``compound insertion order is preserved during roundtrip`` () =
    let tag =
        TagOps.compound
            [ "z-last", Tag.Int 1
              "a-first", Tag.Int 2
              "m-middle", Tag.Int 3 ]

    let bytes = Serialization.toByteArray "root" tag
    let decoded = Serialization.fromByteArray bytes

    Assert.Equal<Tag>(tag, decoded)

[<Fact>]
let ``active patterns and compound getters are usable`` () =
    let tag =
        TagOps.compound
            [ "name", Tag.String "birch"
              "height", Tag.Int 7
              "packed", Tag.LongArray [| 1L; 2L |] ]

    let name = Compound.getString "name" tag
    let height = Compound.getInt "height" tag

    let packedLength =
        match Compound.get "packed" tag with
        | LongArrayTag values -> values.Length
        | _ -> failwith "Expected a long array tag."

    Assert.Equal("birch", name)
    Assert.Equal(7, height)
    Assert.Equal(2, packedLength)

[<Fact>]
let ``compound set replaces key while preserving explicit order`` () =
    let tag =
        TagOps.compound
            [ "a", Tag.Int 1
              "b", Tag.Int 2 ]
        |> Compound.set "a" (Tag.Int 3)

    match tag with
    | CompoundTag [ ("b", IntTag 2); ("a", IntTag 3) ] -> ()
    | _ -> failwithf "Unexpected compound order: %A" tag

[<Fact>]
let ``unsupported format failures use typed exception`` () =
    let invalid = [| 255uy; 1uy; 2uy |]

    Assert.Throws<NbtUnsupportedFormatException>(fun () ->
        Serialization.fromByteArrayAuto invalid |> ignore)
    |> ignore

[<Fact>]
let ``policy failures use typed exception`` () =
    let bytes =
        Serialization.toByteArray
            "root"
            (TagOps.compound [ "nested", TagOps.list TagType.Int [ Tag.Int 1; Tag.Int 2 ] ])

    Assert.Throws<NbtPolicyException>(fun () ->
        Serialization.fromByteArrayWithAndPolicy
            ByteOrder.BigEndian
            { ReaderPolicy.Default with
                MaxCollectionLength = Some 1 }
            bytes
        |> ignore)
    |> ignore

[<Fact>]
let ``result access helpers return ok values`` () =
    let tag =
        TagOps.compound
            [ "name", Tag.String "oak"
              "age", Tag.Int 9 ]

    Assert.Equal(Ok "oak", ResultAccess.requireStringAt "name" tag)
    Assert.Equal(Ok 9, ResultAccess.requireIntAt "age" tag)

[<Fact>]
let ``result access helpers report missing keys`` () =
    let tag = TagOps.compound [ "name", Tag.String "oak" ]

    match ResultAccess.requireIntAt "age" tag with
    | Error message -> Assert.Contains("Missing compound key 'age'", message)
    | Ok _ -> failwith "Expected an error result."

[<Fact>]
let ``builder dsl creates compound and list tags`` () =
    let tag =
        compound {
            "name", Tag.String "spruce"
            "values",
                list {
                    Tag.Int 1
                    Tag.Int 2
                    Tag.Int 3
                }
        }

    match tag with
    | CompoundTag [ ("name", StringTag "spruce"); ("values", ListTag (TagType.Int, [ IntTag 1; IntTag 2; IntTag 3 ])) ] -> ()
    | _ -> failwithf "Unexpected builder output: %A" tag

[<Fact>]
let ``snbt parser roundtrips emitted snbt`` () =
    let tag =
        TagOps.compound
            [ "name", Tag.String "birch"
              "height", Tag.Int 12
              "payload", Tag.ByteArray [| 0uy; 255uy |]
              "packed", Tag.LongArray [| 9L; -2L |] ]

    let snbt = TagOps.toSnbt tag
    let parsed = Snbt.parse snbt

    Assert.Equal<Tag>(tag, parsed)

[<Fact>]
let ``snbt parser handles typed arrays and nested compounds`` () =
    let parsed =
        Snbt.parse """{name:"oak", bytes:[B;1B, -1B], ints:[I;1, 2], longs:[L;3L, -4L], nested:{flag:1b}}"""

    match parsed with
    | CompoundTag entries ->
        Assert.Equal(Some(Tag.String "oak"), CompoundEntries.tryFind "name" entries)
        Assert.Equal(Some(Tag.ByteArray [| 1uy; 255uy |]), CompoundEntries.tryFind "bytes" entries)
        Assert.Equal(Some(Tag.IntArray [| 1; 2 |]), CompoundEntries.tryFind "ints" entries)
        Assert.Equal(Some(Tag.LongArray [| 3L; -4L |]), CompoundEntries.tryFind "longs" entries)
    | _ -> failwith "Expected parsed compound."

[<Fact>]
let ``snbt parser handles mojang style numeric and string forms`` () =
    let parsed =
        Snbt.parse """{single:'hi', escaped:"line\n\tindent", hex:0x10, binary:-0b11, decimal:1.5, byte:1b}"""

    match parsed with
    | CompoundTag entries ->
        Assert.Equal(Some(Tag.String "hi"), CompoundEntries.tryFind "single" entries)
        Assert.Equal(Some(Tag.String "line\n\tindent"), CompoundEntries.tryFind "escaped" entries)
        Assert.Equal(Some(Tag.Int 16), CompoundEntries.tryFind "hex" entries)
        Assert.Equal(Some(Tag.Int -3), CompoundEntries.tryFind "binary" entries)
        Assert.Equal(Some(Tag.Double 1.5), CompoundEntries.tryFind "decimal" entries)
        Assert.Equal(Some(Tag.Byte 1y), CompoundEntries.tryFind "byte" entries)
    | _ -> failwith "Expected parsed compound."

[<Fact>]
let ``snbt parser accepts relaxed unquoted keys and empty typed arrays`` () =
    let parsed =
        Snbt.parse """{unicode test:aé日𐐁, empty_byte_array:[B;], empty_int_array:[I;], empty_long_array:[L;], this is a key:hello}"""

    match parsed with
    | CompoundTag entries ->
        Assert.Equal(Some(Tag.String "aé日𐐁"), CompoundEntries.tryFind "unicode test" entries)
        Assert.Equal(Some(Tag.ByteArray [||]), CompoundEntries.tryFind "empty_byte_array" entries)
        Assert.Equal(Some(Tag.IntArray [||]), CompoundEntries.tryFind "empty_int_array" entries)
        Assert.Equal(Some(Tag.LongArray [||]), CompoundEntries.tryFind "empty_long_array" entries)
        Assert.Equal(Some(Tag.String "hello"), CompoundEntries.tryFind "this is a key" entries)
    | _ -> failwith "Expected parsed compound."

[<Fact>]
let ``snbt parser accepts relaxed unquoted string values with nested braces`` () =
    let parsed =
        Snbt.parse """{Command:tellraw @p {text:'Nothing happened, sorry! :)',color:gray}, Time:1}"""

    match parsed with
    | CompoundTag entries ->
        Assert.Equal(
            Some(Tag.String "tellraw @p {text:'Nothing happened, sorry! :)',color:gray}"),
            CompoundEntries.tryFind "Command" entries
        )
        Assert.Equal(Some(Tag.Int 1), CompoundEntries.tryFind "Time" entries)
    | _ -> failwith "Expected parsed compound."
