module Wabbit.Codec.NBT.CorpusTests

open System
open System.IO
open Wabbit.Codec.NBT
open Xunit

type private SampleCase =
    { RelativePath: string
      Format: NbtFormat
      Compression: Compression }

let private sampleRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "..", "..", "data-nbt-samples"))

let private samplePath (relativePath: string) =
    let segments = relativePath.Split('/')
    Path.Combine(sampleRoot, Path.Combine(segments))

let private readBytes relativePath = File.ReadAllBytes(samplePath relativePath)

let private loadNamed sample =
    readBytes sample.RelativePath
    |> Serialization.fromCompressedByteArrayWithFormat sample.Format sample.Compression

let private loadRaw format relativePath =
    readBytes relativePath |> Serialization.fromRawByteArrayWithFormat format

let private expectCompound =
    function
    | Tag.Compound values -> values
    | value -> failwithf "Expected compound tag, got %A" value

let private helloWorldCases =
    [ { RelativePath = "upstream/Hephaistos/common/src/test/resources/hello_world.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.None }
      { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_nbt/hello_world.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.None }
      { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_compressed_nbt/hello_world.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.GZip }
      { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/little_endian_nbt/hello_world.nbt"
        Format = NbtFormat.LittleEndian
        Compression = Compression.None } ]

let private bigTestCases =
    [ { RelativePath = "upstream/Hephaistos/common/src/test/resources/bigtest.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.GZip }
      { RelativePath = "upstream/prismarine-nbt/sample/bigtest.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.None }
      { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_nbt/bigtest.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.None }
      { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_compressed_nbt/bigtest.nbt"
        Format = NbtFormat.BigEndian
        Compression = Compression.GZip }
      { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/little_endian_nbt/bigtest.nbt"
        Format = NbtFormat.LittleEndian
        Compression = Compression.None } ]

let private levelCaseTriples =
    [ [ { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_nbt/B1_11_level.nbt"
          Format = NbtFormat.BigEndian
          Compression = Compression.None }
        { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_compressed_nbt/B1_11_level.nbt"
          Format = NbtFormat.BigEndian
          Compression = Compression.GZip }
        { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/little_endian_nbt/B1_11_level.nbt"
          Format = NbtFormat.LittleEndian
          Compression = Compression.None } ]
      [ { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_nbt/J1_12_level.nbt"
          Format = NbtFormat.BigEndian
          Compression = Compression.None }
        { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_compressed_nbt/J1_12_level.nbt"
          Format = NbtFormat.BigEndian
          Compression = Compression.GZip }
        { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/little_endian_nbt/J1_12_level.nbt"
          Format = NbtFormat.LittleEndian
          Compression = Compression.None } ]
      [ { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_nbt/J1_13_level.nbt"
          Format = NbtFormat.BigEndian
          Compression = Compression.None }
        { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/big_endian_compressed_nbt/J1_13_level.nbt"
          Format = NbtFormat.BigEndian
          Compression = Compression.GZip }
        { RelativePath = "upstream/Amulet-NBT/tests/test_amulet_nbt/test_read_file/src/little_endian_nbt/J1_13_level.nbt"
          Format = NbtFormat.LittleEndian
          Compression = Compression.None } ] ]

[<Fact>]
let ``sample corpus directory exists`` () =
    Assert.True(Directory.Exists sampleRoot, $"Missing sample corpus directory: {sampleRoot}")

[<Fact>]
let ``hello world fixtures decode to the same compound`` () =
    let decoded = helloWorldCases |> List.map loadNamed
    let baseline = decoded.Head

    for tag in decoded.Tail do
        Assert.Equal<Tag>(baseline, tag)

    let values = expectCompound baseline
    Assert.Equal(Some(Tag.String "Bananrama"), CompoundEntries.tryFind "name" values)

[<Fact>]
let ``bigtest fixtures agree across repos encodings and compression`` () =
    let decoded = bigTestCases |> List.map loadNamed
    let baseline = decoded.Head

    for tag in decoded.Tail do
        Assert.Equal<Tag>(baseline, tag)

    let values = expectCompound baseline
    Assert.True(values.Length > 0, "Expected the bigtest fixture to be a non-empty compound.")

[<Fact>]
let ``level fixtures agree across big endian little endian and gzip variants`` () =
    for cases in levelCaseTriples do
        let decoded = cases |> List.map loadNamed
        let baseline = decoded.Head

        for tag in decoded.Tail do
            Assert.Equal<Tag>(baseline, tag)

        let values = expectCompound baseline
        Assert.True(values.Length > 0, $"Expected non-empty compound for {cases.Head.RelativePath}")

[<Fact>]
let ``classic schematic fixtures decode as compounds with dimensions`` () =
    let schematicCases =
        [ { RelativePath = "upstream/nbtschematic/tests/test_schematic/simple.schematic"
            Format = NbtFormat.BigEndian
            Compression = Compression.GZip }
          { RelativePath = "upstream/nbtschematic/tests/test_schematic/counting.schematic"
            Format = NbtFormat.BigEndian
            Compression = Compression.GZip } ]

    for sample in schematicCases do
        let values = loadNamed sample |> expectCompound
        Assert.True(CompoundEntries.containsKey "Width" values, $"Missing Width in {sample.RelativePath}")
        Assert.True(CompoundEntries.containsKey "Height" values, $"Missing Height in {sample.RelativePath}")
        Assert.True(CompoundEntries.containsKey "Length" values, $"Missing Length in {sample.RelativePath}")

[<Fact>]
let ``auto detection decodes gzip named nbt`` () =
    let bytes = readBytes "upstream/Hephaistos/common/src/test/resources/bigtest.nbt"
    let autoDecoded = Serialization.fromByteArrayAuto bytes
    let explicitDecoded = Serialization.fromCompressedByteArray Compression.GZip bytes
    Assert.Equal<Tag>(explicitDecoded, autoDecoded)

[<Fact>]
let ``auto detection decodes little varint named nbt`` () =
    let bytes = readBytes "upstream/prismarine-nbt/sample/biome_definitions.le.nbt"
    let autoDecoded = Serialization.fromByteArrayAuto bytes
    let explicitDecoded = Serialization.fromByteArrayWithFormat NbtFormat.LittleEndianVarInt bytes
    Assert.Equal<Tag>(explicitDecoded, autoDecoded)

[<Fact>]
let ``little varint stream can read sequential named tags`` () =
    let bytes = readBytes "upstream/prismarine-nbt/sample/block_states.lev.nbt"

    use stream = new MemoryStream(bytes, writable = false)
    use reader = new NBTInputStream(stream, format = NbtFormat.LittleEndianVarInt)

    let first = reader.ReadNamedTag()
    let second = reader.ReadNamedTag()

    match first.Tag, second.Tag with
    | Tag.Compound firstValues, Tag.Compound secondValues ->
        Assert.True(firstValues.Length > 0, "Expected the first little-varint tag to be non-empty.")
        Assert.True(secondValues.Length > 0, "Expected the second little-varint tag to be non-empty.")
        Assert.Equal("", first.Name)
        Assert.Equal("", second.Name)
    | _ -> failwith "Expected named compound tags from the little-varint fixture."

[<Fact>]
let ``decoder rejects malformed or non single-document fixture payloads`` () =
    let emptyBytes = readBytes "upstream/Amulet-NBT/tests/test_load/empty.nbt"
    Assert.ThrowsAny<Exception>(fun () -> Serialization.fromRawByteArray emptyBytes |> ignore) |> ignore

    let repeatedRaw = readBytes "upstream/Amulet-NBT/tests/test_load/array.nbt"
    Assert.ThrowsAny<Exception>(fun () -> Serialization.fromRawByteArray repeatedRaw |> ignore) |> ignore

    let littleVarIntLike = readBytes "upstream/prismarine-nbt/sample/block_states.lev.nbt"
    Assert.ThrowsAny<Exception>(fun () -> Serialization.fromByteArrayAuto littleVarIntLike |> ignore) |> ignore

    let emptyNamedCompound = readBytes "upstream/prismarine-nbt/sample/emptyComp.nbt"
    let tag = Serialization.fromByteArrayAuto emptyNamedCompound

    match tag with
    | Tag.Compound [] -> ()
    | _ -> failwith "Expected an empty named compound."

[<Fact>]
let ``reader policy can reject large fixture collections`` () =
    let bytes = readBytes "upstream/prismarine-nbt/sample/bigtest.nbt"

    Assert.ThrowsAny<Exception>(fun () ->
        Serialization.fromByteArrayWithFormatAndPolicy
            NbtFormat.BigEndian
            { ReaderPolicy.Default with
                MaxCollectionLength = Some 1 }
            bytes
        |> ignore)
    |> ignore
