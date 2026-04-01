namespace Wabbit.Codec.NBT

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Runtime.CompilerServices
open System.Text

type NbtDecodeException(message: string, ?innerException: exn) =
    inherit Exception(message, defaultArg innerException null)

[<Sealed>]
type NbtPolicyException(message: string, ?innerException: exn) =
    inherit NbtDecodeException(message, defaultArg innerException null)

[<Sealed>]
type NbtUnsupportedFormatException(message: string, ?innerException: exn) =
    inherit NbtDecodeException(message, defaultArg innerException null)

[<RequireQualifiedAccess>]
type ByteOrder =
    | BigEndian
    | LittleEndian

[<RequireQualifiedAccess>]
type NbtFormat =
    | BigEndian
    | LittleEndian
    | LittleEndianVarInt

[<RequireQualifiedAccess>]
module NbtFormat =
    let ofByteOrder =
        function
        | ByteOrder.BigEndian -> NbtFormat.BigEndian
        | ByteOrder.LittleEndian -> NbtFormat.LittleEndian

[<RequireQualifiedAccess>]
type Compression =
    | None
    | GZip
    | ZLib

type ReaderPolicy =
    { MaxDepth: int option
      MaxBytes: int64 option
      MaxStringBytes: int option
      MaxCollectionLength: int option
      MaxCompoundEntries: int option }
    static member Default =
        { MaxDepth = None
          MaxBytes = None
          MaxStringBytes = None
          MaxCollectionLength = None
          MaxCompoundEntries = None }

[<RequireQualifiedAccess>]
type TagType =
    | End = 0
    | Byte = 1
    | Short = 2
    | Int = 3
    | Long = 4
    | Float = 5
    | Double = 6
    | ByteArray = 7
    | String = 8
    | List = 9
    | Compound = 10
    | IntArray = 11
    | LongArray = 12

type CompoundEntries = (string * Tag) list

and [<RequireQualifiedAccess>] Tag =
    | End
    | Byte of sbyte
    | Short of int16
    | Int of int32
    | Long of int64
    | Float of single
    | Double of double
    | ByteArray of byte[]
    | String of string
    | List of elementType: TagType * values: Tag list
    | Compound of values: CompoundEntries
    | IntArray of int[]
    | LongArray of int64[]

type NamedTag =
    { Name: string
      Tag: Tag }

[<RequireQualifiedAccess>]
module private Errors =
    let decode message = NbtDecodeException(message) :> exn
    let policy message = NbtPolicyException(message) :> exn
    let unsupported message = NbtUnsupportedFormatException(message) :> exn

[<RequireQualifiedAccess>]
module TagType =
    let all =
        [ TagType.End
          TagType.Byte
          TagType.Short
          TagType.Int
          TagType.Long
          TagType.Float
          TagType.Double
          TagType.ByteArray
          TagType.String
          TagType.List
          TagType.Compound
          TagType.IntArray
          TagType.LongArray ]

    let name =
        function
        | TagType.End -> "TAG_End"
        | TagType.Byte -> "TAG_Byte"
        | TagType.Short -> "TAG_Short"
        | TagType.Int -> "TAG_Int"
        | TagType.Long -> "TAG_Long"
        | TagType.Float -> "TAG_Float"
        | TagType.Double -> "TAG_Double"
        | TagType.ByteArray -> "TAG_Byte_Array"
        | TagType.String -> "TAG_String"
        | TagType.List -> "TAG_List"
        | TagType.Compound -> "TAG_Compound"
        | TagType.IntArray -> "TAG_Int_Array"
        | TagType.LongArray -> "TAG_Long_Array"
        | value -> invalidArg (nameof value) $"Invalid tag type (%d{int value})"

    let tryOfCode code =
        match code with
        | 0 -> Some TagType.End
        | 1 -> Some TagType.Byte
        | 2 -> Some TagType.Short
        | 3 -> Some TagType.Int
        | 4 -> Some TagType.Long
        | 5 -> Some TagType.Float
        | 6 -> Some TagType.Double
        | 7 -> Some TagType.ByteArray
        | 8 -> Some TagType.String
        | 9 -> Some TagType.List
        | 10 -> Some TagType.Compound
        | 11 -> Some TagType.IntArray
        | 12 -> Some TagType.LongArray
        | _ -> None

    let ofCode code =
        match tryOfCode code with
        | Some value -> value
        | None -> invalidArg (nameof code) $"Invalid tag code (%d{code})"

[<RequireQualifiedAccess>]
module CompoundEntries =
    let empty : CompoundEntries = []

    let ofSeq (values: seq<string * Tag>) : CompoundEntries =
        List.ofSeq values

    let count (values: CompoundEntries) = values.Length

    let tryFind key (values: CompoundEntries) =
        values
        |> List.rev
        |> List.tryPick (fun (candidateKey, value) ->
            if candidateKey = key then Some value else None)

    let containsKey key values = tryFind key values |> Option.isSome

    let add key value values : CompoundEntries =
        values @ [ key, value ]

    let set key value values : CompoundEntries =
        values
        |> List.filter (fun (candidateKey, _) -> candidateKey <> key)
        |> add key value

    let remove key values : CompoundEntries =
        values |> List.filter (fun (candidateKey, _) -> candidateKey <> key)

    let keys values = values |> List.map fst
    let values values = values |> List.map snd

[<RequireQualifiedAccess>]
module TagOps =
    let tagType =
        function
        | Tag.End -> TagType.End
        | Tag.Byte _ -> TagType.Byte
        | Tag.Short _ -> TagType.Short
        | Tag.Int _ -> TagType.Int
        | Tag.Long _ -> TagType.Long
        | Tag.Float _ -> TagType.Float
        | Tag.Double _ -> TagType.Double
        | Tag.ByteArray _ -> TagType.ByteArray
        | Tag.String _ -> TagType.String
        | Tag.List _ -> TagType.List
        | Tag.Compound _ -> TagType.Compound
        | Tag.IntArray _ -> TagType.IntArray
        | Tag.LongArray _ -> TagType.LongArray

    let isListOf elementType =
        function
        | Tag.List (TagType.End, []) -> true
        | Tag.List (actualType, _) when actualType = elementType -> true
        | _ -> false

    let tryListItems elementType =
        function
        | Tag.List (TagType.End, []) -> Some []
        | Tag.List (actualType, values) when actualType = elementType -> Some values
        | _ -> None

    let list elementType values =
        let values = List.ofSeq values

        if elementType = TagType.End && not values.IsEmpty then
            invalidArg (nameof values) "TAG_End lists cannot contain elements."

        if values |> List.exists (fun value -> tagType value <> elementType) then
            invalidArg (nameof values) "NBT lists must be homogeneous."

        Tag.List (elementType, values)

    let listOf values =
        let values = List.ofSeq values

        match values with
        | [] -> Tag.List (TagType.End, [])
        | head :: _ -> list (tagType head) values

    let compound values =
        values |> CompoundEntries.ofSeq |> Tag.Compound

    let emptyCompound = Tag.Compound CompoundEntries.empty

    let private appendNumeric (value: IFormattable) (suffix: string) (builder: StringBuilder) =
        builder.Append(value.ToString(null, CultureInfo.InvariantCulture)).Append(suffix) |> ignore

    let rec appendSnbt (builder: StringBuilder) tag =
        match tag with
        | Tag.Compound values ->
            builder.Append('{') |> ignore

            values
            |> Seq.iteri (fun index (key, value) ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                builder.Append(key).Append(": ") |> ignore
                appendSnbt builder value)

            builder.Append('}') |> ignore
        | Tag.List (_, values) ->
            builder.Append('[') |> ignore

            values
            |> Seq.iteri (fun index value ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                appendSnbt builder value)

            builder.Append(']') |> ignore
        | Tag.ByteArray values ->
            builder.Append("[B;") |> ignore

            values
            |> Seq.iteri (fun index value ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                builder.Append(int (sbyte value)).Append('B') |> ignore)

            builder.Append(']') |> ignore
        | Tag.IntArray values ->
            builder.Append("[I;") |> ignore

            values
            |> Seq.iteri (fun index value ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                builder.Append(value) |> ignore)

            builder.Append(']') |> ignore
        | Tag.LongArray values ->
            builder.Append("[L;") |> ignore

            values
            |> Seq.iteri (fun index value ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                builder.Append(value.ToString(CultureInfo.InvariantCulture)).Append('L') |> ignore)

            builder.Append(']') |> ignore
        | Tag.String value ->
            builder.Append('"') |> ignore

            for character in value do
                match character with
                | '\\' -> builder.Append("\\\\") |> ignore
                | '"' -> builder.Append("\\\"") |> ignore
                | '\b' -> builder.Append("\\b") |> ignore
                | '\t' -> builder.Append("\\t") |> ignore
                | '\n' -> builder.Append("\\n") |> ignore
                | '\u000C' -> builder.Append("\\f") |> ignore
                | '\r' -> builder.Append("\\r") |> ignore
                | _ -> builder.Append(character) |> ignore

            builder.Append('"') |> ignore
        | Tag.Byte value -> appendNumeric value "b" builder
        | Tag.Short value -> appendNumeric value "s" builder
        | Tag.Int value -> builder.Append(value.ToString(CultureInfo.InvariantCulture)) |> ignore
        | Tag.Long value -> appendNumeric value "l" builder
        | Tag.Float value -> appendNumeric value "f" builder
        | Tag.Double value -> appendNumeric value "d" builder
        | Tag.End -> builder.Append("END") |> ignore

    let toSnbt tag =
        let builder = StringBuilder()
        appendSnbt builder tag
        builder.ToString()

    let tryAsCompound =
        function
        | Tag.Compound entries -> Some entries
        | _ -> None

    let tryAsList =
        function
        | Tag.List (elementType, values) -> Some (elementType, values)
        | _ -> None

    let tryAsString =
        function
        | Tag.String value -> Some value
        | _ -> None

    let tryAsInt =
        function
        | Tag.Int value -> Some value
        | _ -> None

    let tryAsLongArray =
        function
        | Tag.LongArray value -> Some value
        | _ -> None

[<AutoOpen>]
module TagPatterns =
    let (|EndTag|_|) =
        function
        | Tag.End -> Some ()
        | _ -> None

    let (|ByteTag|_|) =
        function
        | Tag.Byte value -> Some value
        | _ -> None

    let (|ShortTag|_|) =
        function
        | Tag.Short value -> Some value
        | _ -> None

    let (|IntTag|_|) =
        function
        | Tag.Int value -> Some value
        | _ -> None

    let (|LongTag|_|) =
        function
        | Tag.Long value -> Some value
        | _ -> None

    let (|FloatTag|_|) =
        function
        | Tag.Float value -> Some value
        | _ -> None

    let (|DoubleTag|_|) =
        function
        | Tag.Double value -> Some value
        | _ -> None

    let (|ByteArrayTag|_|) =
        function
        | Tag.ByteArray value -> Some value
        | _ -> None

    let (|StringTag|_|) =
        function
        | Tag.String value -> Some value
        | _ -> None

    let (|ListTag|_|) =
        function
        | Tag.List (elementType, values) -> Some (elementType, values)
        | _ -> None

    let (|CompoundTag|_|) =
        function
        | Tag.Compound values -> Some values
        | _ -> None

    let (|IntArrayTag|_|) =
        function
        | Tag.IntArray value -> Some value
        | _ -> None

    let (|LongArrayTag|_|) =
        function
        | Tag.LongArray value -> Some value
        | _ -> None

[<RequireQualifiedAccess>]
module Compound =
    let private tryFind key =
        function
        | Tag.Compound values -> CompoundEntries.tryFind key values
        | _ -> None

    let entries =
        function
        | Tag.Compound values -> values
        | _ -> []

    let empty = Tag.Compound CompoundEntries.empty
    let ofSeq values = values |> CompoundEntries.ofSeq |> Tag.Compound
    let keys tag = entries tag |> CompoundEntries.keys
    let values tag = entries tag |> CompoundEntries.values
    let containsKey key tag = tryFind key tag |> Option.isSome
    let tryGet key tag = tryFind key tag
    let tryGetByteArray key tag = tryFind key tag |> Option.bind (function Tag.ByteArray value -> Some value | _ -> None)
    let tryGetIntArray key tag = tryFind key tag |> Option.bind (function Tag.IntArray value -> Some value | _ -> None)
    let tryGetLongArray key tag = tryFind key tag |> Option.bind (function Tag.LongArray value -> Some value | _ -> None)
    let tryGetByte key tag = tryFind key tag |> Option.bind (function Tag.Byte value -> Some value | _ -> None)
    let tryGetShort key tag = tryFind key tag |> Option.bind (function Tag.Short value -> Some value | _ -> None)
    let tryGetInt key tag = tryFind key tag |> Option.bind (function Tag.Int value -> Some value | _ -> None)
    let tryGetLong key tag = tryFind key tag |> Option.bind (function Tag.Long value -> Some value | _ -> None)
    let tryGetFloat key tag = tryFind key tag |> Option.bind (function Tag.Float value -> Some value | _ -> None)
    let tryGetDouble key tag = tryFind key tag |> Option.bind (function Tag.Double value -> Some value | _ -> None)
    let tryGetString key tag = tryFind key tag |> Option.bind (function Tag.String value -> Some value | _ -> None)
    let tryGetEntries key tag = tryFind key tag |> Option.bind (function Tag.Compound value -> Some value | _ -> None)

    let tryGetList key elementType tag =
        tryFind key tag |> Option.bind (TagOps.tryListItems elementType)

    let private required key expected =
        raise (KeyNotFoundException($"Expected {expected} at compound key '{key}'."))

    let get key tag = tryGet key tag |> Option.defaultWith (fun () -> required key "tag")
    let getByte key tag = tryGetByte key tag |> Option.defaultWith (fun () -> required key "byte")
    let getShort key tag = tryGetShort key tag |> Option.defaultWith (fun () -> required key "short")
    let getInt key tag = tryGetInt key tag |> Option.defaultWith (fun () -> required key "int")
    let getLong key tag = tryGetLong key tag |> Option.defaultWith (fun () -> required key "long")
    let getFloat key tag = tryGetFloat key tag |> Option.defaultWith (fun () -> required key "float")
    let getDouble key tag = tryGetDouble key tag |> Option.defaultWith (fun () -> required key "double")
    let getString key tag = tryGetString key tag |> Option.defaultWith (fun () -> required key "string")
    let getByteArray key tag = tryGetByteArray key tag |> Option.defaultWith (fun () -> required key "byte array")
    let getIntArray key tag = tryGetIntArray key tag |> Option.defaultWith (fun () -> required key "int array")
    let getLongArray key tag = tryGetLongArray key tag |> Option.defaultWith (fun () -> required key "long array")
    let getEntries key tag = tryGetEntries key tag |> Option.defaultWith (fun () -> required key "compound")

    let add key value =
        function
        | Tag.Compound values -> CompoundEntries.add key value values |> Tag.Compound
        | _ -> invalidArg (nameof value) "Expected compound tag."

    let set key value =
        function
        | Tag.Compound values -> CompoundEntries.set key value values |> Tag.Compound
        | _ -> invalidArg (nameof value) "Expected compound tag."

    let remove key =
        function
        | Tag.Compound values -> CompoundEntries.remove key values |> Tag.Compound
        | _ -> invalidArg (nameof key) "Expected compound tag."

[<RequireQualifiedAccess>]
module ResultAccess =
    let private wrongType expected actual =
        Error $"Expected {expected}, got {TagType.name (TagOps.tagType actual)}"

    let requireCompound =
        function
        | Tag.Compound entries -> Ok entries
        | tag -> wrongType "compound" tag

    let requireString =
        function
        | Tag.String value -> Ok value
        | tag -> wrongType "string" tag

    let requireInt =
        function
        | Tag.Int value -> Ok value
        | tag -> wrongType "int" tag

    let requireLongArray =
        function
        | Tag.LongArray value -> Ok value
        | tag -> wrongType "long array" tag

    let tryGetAs key projector tag =
        match Compound.tryGet key tag with
        | None -> Error $"Missing compound key '{key}'"
        | Some value -> projector value

    let requireStringAt key tag = tryGetAs key requireString tag
    let requireIntAt key tag = tryGetAs key requireInt tag
    let requireCompoundAt key tag = tryGetAs key requireCompound tag
    let requireLongArrayAt key tag = tryGetAs key requireLongArray tag

[<AutoOpen>]
module Builders =
    type CompoundBuilder() =
        member _.Yield(()) : CompoundEntries = []
        member _.Yield((key: string, value: Tag)) : CompoundEntries = [ key, value ]
        member _.Delay(f: unit -> CompoundEntries) = f
        member _.Run(f: unit -> CompoundEntries) = Tag.Compound(f ())
        member _.Combine(left: CompoundEntries, right: unit -> CompoundEntries) = left @ right ()
        member _.For(source: seq<'T>, body: 'T -> CompoundEntries) = source |> Seq.toList |> List.collect body
        member _.Zero() : CompoundEntries = []

    type ListBuilder() =
        member _.Yield(tag: Tag) : Tag list = [ tag ]
        member _.YieldFrom(tags: seq<Tag>) : Tag list = List.ofSeq tags
        member _.Delay(f: unit -> Tag list) = f
        member _.Run(f: unit -> Tag list) = TagOps.listOf (f ())
        member _.Combine(left: Tag list, right: unit -> Tag list) = left @ right ()
        member _.For(source: seq<'T>, body: 'T -> Tag list) = source |> Seq.toList |> List.collect body
        member _.Zero() : Tag list = []

    let compound = CompoundBuilder()
    let list = ListBuilder()

[<AbstractClass; Sealed>]
type private VarInt =
    static member ReadUnsigned32(readByte: unit -> byte) =
        let mutable result = 0u
        let mutable shift = 0
        let mutable continueReading = true

        while continueReading do
            if shift >= 35 then
                raise (Errors.decode "VarInt32 is too long.")

            let value = readByte ()
            result <- result ||| (uint32 (value &&& 0x7Fuy) <<< shift)
            continueReading <- (value &&& 0x80uy) <> 0uy
            shift <- shift + 7

        result

    static member ReadUnsigned64(readByte: unit -> byte) =
        let mutable result = 0UL
        let mutable shift = 0
        let mutable continueReading = true

        while continueReading do
            if shift >= 70 then
                raise (Errors.decode "VarInt64 is too long.")

            let value = readByte ()
            result <- result ||| (uint64 (value &&& 0x7Fuy) <<< shift)
            continueReading <- (value &&& 0x80uy) <> 0uy
            shift <- shift + 7

        result

    static member WriteUnsigned32(writeByte: byte -> unit, value: uint32) =
        let mutable remaining = value

        while remaining >= 0x80u do
            writeByte (byte ((remaining &&& 0x7Fu) ||| 0x80u))
            remaining <- remaining >>> 7

        writeByte (byte remaining)

    static member WriteUnsigned64(writeByte: byte -> unit, value: uint64) =
        let mutable remaining = value

        while remaining >= 0x80UL do
            writeByte (byte ((remaining &&& 0x7FUL) ||| 0x80UL))
            remaining <- remaining >>> 7

        writeByte (byte remaining)

    static member DecodeZigZag32(value: uint32) =
        int32 ((value >>> 1) ^^^ uint32 (-(int32 (value &&& 1u))))

    static member DecodeZigZag64(value: uint64) =
        int64 ((value >>> 1) ^^^ uint64 (-(int64 (value &&& 1UL))))

    static member EncodeZigZag32(value: int32) =
        uint32 ((value <<< 1) ^^^ (value >>> 31))

    static member EncodeZigZag64(value: int64) =
        uint64 ((value <<< 1) ^^^ (value >>> 63))

type internal ReadCodec(stream: Stream, format: NbtFormat, policy: ReaderPolicy) =
    let mutable totalBytesRead = 0L
    let scratch = Array.zeroCreate<byte> 8192

    let checkBytes count =
        totalBytesRead <- totalBytesRead + int64 count

        match policy.MaxBytes with
        | Some maxBytes when totalBytesRead > maxBytes ->
            raise (Errors.policy $"Reader policy exceeded maximum byte budget of {maxBytes}.")
        | _ -> ()

    let readExact count =
        let buffer = Array.zeroCreate<byte> count
        let mutable offset = 0

        while offset < count do
            let bytesRead = stream.Read(buffer, offset, count - offset)

            if bytesRead = 0 then
                raise (EndOfStreamException())

            offset <- offset + bytesRead
            checkBytes bytesRead

        buffer

    let readWith reader count = readExact count |> reader

    let skipExact count =
        let mutable remaining = count

        while remaining > 0 do
            let chunk = min remaining scratch.Length
            let bytesRead = stream.Read(scratch, 0, chunk)

            if bytesRead = 0 then
                raise (EndOfStreamException())

            remaining <- remaining - bytesRead
            checkBytes bytesRead

    member _.ReadByte() =
        let value = stream.ReadByte()

        if value < 0 then
            raise (EndOfStreamException())

        checkBytes 1
        byte value

    member this.ReadSByte() = this.ReadByte() |> sbyte

    member _.ReadUInt16() =
        readWith
            (if format = NbtFormat.BigEndian then
                 BinaryPrimitives.ReadUInt16BigEndian
             else
                 BinaryPrimitives.ReadUInt16LittleEndian)
            2

    member _.ReadInt16() =
        readWith
            (if format = NbtFormat.BigEndian then
                 BinaryPrimitives.ReadInt16BigEndian
             else
                 BinaryPrimitives.ReadInt16LittleEndian)
            2

    member _.ReadInt32() =
        readWith
            (if format = NbtFormat.BigEndian then
                 BinaryPrimitives.ReadInt32BigEndian
             else
                 BinaryPrimitives.ReadInt32LittleEndian)
            4

    member _.ReadInt64() =
        readWith
            (if format = NbtFormat.BigEndian then
                 BinaryPrimitives.ReadInt64BigEndian
             else
                 BinaryPrimitives.ReadInt64LittleEndian)
            8

    member _.ReadSingle() =
        readWith
            (if format = NbtFormat.BigEndian then
                 BinaryPrimitives.ReadSingleBigEndian
             else
                 BinaryPrimitives.ReadSingleLittleEndian)
            4

    member _.ReadDouble() =
        readWith
            (if format = NbtFormat.BigEndian then
                 BinaryPrimitives.ReadDoubleBigEndian
             else
                 BinaryPrimitives.ReadDoubleLittleEndian)
            8

    member _.ReadBytes count = readExact count
    member _.SkipBytes count = skipExact count
    member this.ReadVarUInt32() = VarInt.ReadUnsigned32(this.ReadByte)
    member this.ReadVarUInt64() = VarInt.ReadUnsigned64(this.ReadByte)
    member this.ReadZigZag32() = this.ReadVarUInt32() |> VarInt.DecodeZigZag32
    member this.ReadZigZag64() = this.ReadVarUInt64() |> VarInt.DecodeZigZag64

type internal WriteCodec(stream: Stream, format: NbtFormat) =
    let writeWith count writer =
        let buffer = Array.zeroCreate<byte> count
        writer buffer
        stream.Write(buffer, 0, buffer.Length)

    member _.WriteByte(value: byte) =
        stream.WriteByte(value)

    member this.WriteSByte(value: sbyte) = this.WriteByte(byte value)

    member _.WriteUInt16(value: uint16) =
        writeWith
            2
            (if format = NbtFormat.BigEndian then
                 fun buffer -> BinaryPrimitives.WriteUInt16BigEndian(buffer, value)
             else
                 fun buffer -> BinaryPrimitives.WriteUInt16LittleEndian(buffer, value))

    member _.WriteInt16(value: int16) =
        writeWith
            2
            (if format = NbtFormat.BigEndian then
                 fun buffer -> BinaryPrimitives.WriteInt16BigEndian(buffer, value)
             else
                 fun buffer -> BinaryPrimitives.WriteInt16LittleEndian(buffer, value))

    member _.WriteInt32(value: int32) =
        writeWith
            4
            (if format = NbtFormat.BigEndian then
                 fun buffer -> BinaryPrimitives.WriteInt32BigEndian(buffer, value)
             else
                 fun buffer -> BinaryPrimitives.WriteInt32LittleEndian(buffer, value))

    member _.WriteInt64(value: int64) =
        writeWith
            8
            (if format = NbtFormat.BigEndian then
                 fun buffer -> BinaryPrimitives.WriteInt64BigEndian(buffer, value)
             else
                 fun buffer -> BinaryPrimitives.WriteInt64LittleEndian(buffer, value))

    member _.WriteSingle(value: single) =
        writeWith
            4
            (if format = NbtFormat.BigEndian then
                 fun buffer -> BinaryPrimitives.WriteSingleBigEndian(buffer, value)
             else
                 fun buffer -> BinaryPrimitives.WriteSingleLittleEndian(buffer, value))

    member _.WriteDouble(value: double) =
        writeWith
            8
            (if format = NbtFormat.BigEndian then
                 fun buffer -> BinaryPrimitives.WriteDoubleBigEndian(buffer, value)
             else
                 fun buffer -> BinaryPrimitives.WriteDoubleLittleEndian(buffer, value))

    member _.WriteBytes(value: byte[]) =
        stream.Write(value, 0, value.Length)

    member this.WriteVarUInt32(value: uint32) = VarInt.WriteUnsigned32(this.WriteByte, value)
    member this.WriteVarUInt64(value: uint64) = VarInt.WriteUnsigned64(this.WriteByte, value)
    member this.WriteZigZag32(value: int32) = value |> VarInt.EncodeZigZag32 |> this.WriteVarUInt32
    member this.WriteZigZag64(value: int64) = value |> VarInt.EncodeZigZag64 |> this.WriteVarUInt64

type NBTInputStream
    (
        stream: Stream,
        ?byteOrder: ByteOrder,
        ?format: NbtFormat,
        ?policy: ReaderPolicy
    ) =
    let format =
        match format, byteOrder with
        | Some explicitFormat, _ -> explicitFormat
        | None, Some explicitByteOrder -> NbtFormat.ofByteOrder explicitByteOrder
        | None, None -> NbtFormat.BigEndian

    let policy = defaultArg policy ReaderPolicy.Default
    let codec = ReadCodec(stream, format, policy)
    let encoding = UTF8Encoding(false, true)

    let checkDepth depth =
        match policy.MaxDepth with
        | Some maxDepth when depth > maxDepth ->
            raise (Errors.policy $"Reader policy exceeded maximum depth of {maxDepth}.")
        | _ -> ()

    let checkStringBytes length =
        match policy.MaxStringBytes with
        | Some maxBytes when length > maxBytes ->
            raise (Errors.policy $"Reader policy exceeded maximum string length of {maxBytes} bytes.")
        | _ -> ()

    let checkCollectionLength length =
        match policy.MaxCollectionLength with
        | Some maxLength when length > maxLength ->
            raise (Errors.policy $"Reader policy exceeded maximum collection length of {maxLength}.")
        | _ -> ()

    let checkCompoundEntries count =
        match policy.MaxCompoundEntries with
        | Some maxEntries when count > maxEntries ->
            raise (Errors.policy $"Reader policy exceeded maximum compound entry count of {maxEntries}.")
        | _ -> ()

    let readTagType () =
        let rawType = int (codec.ReadByte())

        match TagType.tryOfCode rawType with
        | Some tagType -> tagType
        | None -> raise (Errors.decode $"Illegal tag type %d{rawType}.")

    let readLength isString =
        if format = NbtFormat.LittleEndianVarInt then
            let length = codec.ReadVarUInt32() |> int
            if isString then checkStringBytes length else checkCollectionLength length
            length
        else
            let length = codec.ReadUInt16() |> int
            if isString then checkStringBytes length
            length

    let readUtf8 length =
        codec.ReadBytes length |> encoding.GetString

    let guardLength length =
        if length < 0 then
            raise (Errors.decode "Negative lengths are not permitted in NBT payloads.")

        checkCollectionLength length
        length

    let readArrayLength () =
        if format = NbtFormat.LittleEndianVarInt then
            codec.ReadZigZag32() |> int |> guardLength
        else
            codec.ReadInt32() |> guardLength

    let readIntPayload () =
        if format = NbtFormat.LittleEndianVarInt then
            codec.ReadZigZag32()
        else
            codec.ReadInt32()

    let readLongPayload () =
        if format = NbtFormat.LittleEndianVarInt then
            codec.ReadZigZag64()
        else
            codec.ReadInt64()

    member this.ReadNamedTag() = this.ReadNamedTag(0)

    member private this.ReadNamedTag(depth: int) =
        checkDepth depth
        let tagType = readTagType ()

        let name =
            if tagType = TagType.End then
                ""
            else
                readLength true |> readUtf8

        { Name = name
          Tag = this.ReadTagPayload(tagType, depth) }

    member private this.ReadTagPayload(tagType: TagType, depth: int) =
        checkDepth depth

        match tagType with
        | TagType.End ->
            if depth = 0 then
                raise (Errors.decode "TAG_End found without a TAG_Compound/TAG_List tag preceding it.")

            Tag.End
        | TagType.Byte -> codec.ReadSByte() |> Tag.Byte
        | TagType.Short -> codec.ReadInt16() |> Tag.Short
        | TagType.Int -> readIntPayload () |> Tag.Int
        | TagType.Long -> readLongPayload () |> Tag.Long
        | TagType.Float -> codec.ReadSingle() |> Tag.Float
        | TagType.Double -> codec.ReadDouble() |> Tag.Double
        | TagType.ByteArray ->
            readArrayLength () |> codec.ReadBytes |> Tag.ByteArray
        | TagType.String ->
            readLength true |> readUtf8 |> Tag.String
        | TagType.List ->
            let childType = readTagType ()
            let length =
                if format = NbtFormat.LittleEndianVarInt then
                    codec.ReadZigZag32() |> int |> guardLength
                else
                    codec.ReadInt32() |> guardLength

            let values =
                [ for _ in 1 .. length do
                      let tag = this.ReadTagPayload(childType, depth + 1)

                      match tag with
                      | Tag.End -> raise (Errors.decode "An actual TAG_End was not expected here.")
                      | _ -> tag ]

            Tag.List (childType, values)
        | TagType.Compound ->
            let rec loop count values =
                checkCompoundEntries count
                let nextType = readTagType ()

                if nextType = TagType.End then
                    values
                else
                    let name = readLength true |> readUtf8
                    let value = this.ReadTagPayload(nextType, depth + 1)
                    loop (count + 1) (CompoundEntries.add name value values)

            loop 0 CompoundEntries.empty |> Tag.Compound
        | TagType.IntArray ->
            let length = readArrayLength ()
            Array.init length (fun _ -> readIntPayload ()) |> Tag.IntArray
        | TagType.LongArray ->
            let length = readArrayLength ()
            Array.init length (fun _ -> readLongPayload ()) |> Tag.LongArray
        | value -> invalidArg (nameof tagType) $"Unsupported tag type (%A{value})"

    member this.ReadRawTag() =
        let tagType = readTagType ()
        this.ReadTagPayload(tagType, 0)

    member _.ReadTagTypeCode() = readTagType ()

    member _.ReadStringPayload() =
        let length = readLength true
        readUtf8 length

    member _.ReadIntLikePayload(tagType: TagType) =
        match tagType with
        | TagType.Byte -> codec.ReadSByte() |> int
        | TagType.Short -> codec.ReadInt16() |> int
        | TagType.Int -> readIntPayload () |> int
        | TagType.Long ->
            let value = readLongPayload ()

            if value < int64 Int32.MinValue || value > int64 Int32.MaxValue then
                raise (Errors.decode $"Integer-like payload {value} is outside Int32 range.")

            int value
        | _ -> raise (Errors.decode $"Tag type {tagType} is not integer-like.")

    member _.ReadByteArrayPayload() =
        let length = readArrayLength ()
        codec.ReadBytes length

    member _.ReadIntArrayPayload() =
        let length = readArrayLength ()
        Array.init length (fun _ -> readIntPayload ())

    member _.ReadLongArrayPayload() =
        let length = readArrayLength ()
        Array.init length (fun _ -> readLongPayload ())

    member _.ReadListHeader() =
        let childType = readTagType ()

        let length =
            if format = NbtFormat.LittleEndianVarInt then
                codec.ReadZigZag32() |> int |> guardLength
            else
                codec.ReadInt32() |> guardLength

        childType, length

    member this.ReadNamedTagHeader() =
        let tagType = readTagType ()

        let name =
            if tagType = TagType.End then
                ""
            else
                readLength true |> readUtf8

        tagType, name

    member this.SkipPayload(tagType: TagType) =
        match tagType with
        | TagType.End -> ()
        | TagType.Byte -> codec.ReadSByte() |> ignore
        | TagType.Short -> codec.ReadInt16() |> ignore
        | TagType.Int -> readIntPayload () |> ignore
        | TagType.Long -> readLongPayload () |> ignore
        | TagType.Float -> codec.ReadSingle() |> ignore
        | TagType.Double -> codec.ReadDouble() |> ignore
        | TagType.ByteArray ->
            let length = readArrayLength ()
            codec.SkipBytes length
        | TagType.String ->
            let length = readLength true
            codec.SkipBytes length
        | TagType.List ->
            let childType, length = this.ReadListHeader()

            for _ in 1 .. length do
                this.SkipPayload childType
        | TagType.Compound ->
            let mutable continueLoop = true

            while continueLoop do
                let nextType = readTagType ()

                if nextType = TagType.End then
                    continueLoop <- false
                else
                    let nameLength = readLength true
                    codec.SkipBytes nameLength
                    this.SkipPayload nextType
        | TagType.IntArray ->
            let length = readArrayLength ()

            for _ in 1 .. length do
                readIntPayload () |> ignore
        | TagType.LongArray ->
            let length = readArrayLength ()

            for _ in 1 .. length do
                readLongPayload () |> ignore

    interface IDisposable with
        member _.Dispose() = stream.Dispose()

type NBTOutputStream(stream: Stream, ?byteOrder: ByteOrder, ?format: NbtFormat) =
    let format =
        match format, byteOrder with
        | Some explicitFormat, _ -> explicitFormat
        | None, Some explicitByteOrder -> NbtFormat.ofByteOrder explicitByteOrder
        | None, None -> NbtFormat.BigEndian

    let codec = WriteCodec(stream, format)
    let encoding = UTF8Encoding(false, true)

    let ensureUInt16Length fieldName (bytes: byte[]) =
        if bytes.Length > int UInt16.MaxValue then
            invalidArg fieldName "NBT strings cannot exceed UInt16.MaxValue bytes."

        uint16 bytes.Length

    let writeStringLength fieldName (bytes: byte[]) =
        if format = NbtFormat.LittleEndianVarInt then
            codec.WriteVarUInt32(uint32 bytes.Length)
        else
            codec.WriteUInt16(ensureUInt16Length fieldName bytes)

    let writeArrayLength length =
        if format = NbtFormat.LittleEndianVarInt then
            codec.WriteZigZag32(length)
        else
            codec.WriteInt32(length)

    let writeIntPayload value =
        if format = NbtFormat.LittleEndianVarInt then
            codec.WriteZigZag32(value)
        else
            codec.WriteInt32(value)

    let writeLongPayload value =
        if format = NbtFormat.LittleEndianVarInt then
            codec.WriteZigZag64(value)
        else
            codec.WriteInt64(value)

    member this.WriteNamedTag(name: string, tag: Tag) =
        let tagType = TagOps.tagType tag
        codec.WriteByte(byte (int tagType))

        if tagType = TagType.End then
            raise (Errors.decode "Named TAG_End not permitted.")

        let nameBytes = encoding.GetBytes name
        writeStringLength (nameof name) nameBytes
        codec.WriteBytes(nameBytes)
        this.WriteTagPayload(tag)

    member private this.WriteTagPayload(tag: Tag) =
        match tag with
        | Tag.End -> ()
        | Tag.Byte value -> codec.WriteSByte(value)
        | Tag.Short value -> codec.WriteInt16(value)
        | Tag.Int value -> writeIntPayload value
        | Tag.Long value -> writeLongPayload value
        | Tag.Float value -> codec.WriteSingle(value)
        | Tag.Double value -> codec.WriteDouble(value)
        | Tag.ByteArray value ->
            writeArrayLength value.Length
            codec.WriteBytes(value)
        | Tag.String value ->
            let bytes = encoding.GetBytes value
            writeStringLength "tag" bytes
            codec.WriteBytes(bytes)
        | Tag.List (elementType, values) ->
            codec.WriteByte(byte (int elementType))
            writeArrayLength values.Length

            for value in values do
                this.WriteTagPayload(value)
        | Tag.Compound values ->
            for (key, value) in values do
                this.WriteNamedTag(key, value)

            codec.WriteByte(0uy)
        | Tag.IntArray value ->
            writeArrayLength value.Length

            for item in value do
                writeIntPayload item
        | Tag.LongArray value ->
            writeArrayLength value.Length

            for item in value do
                writeLongPayload item

    member this.WriteRawTag(tag: Tag) =
        codec.WriteByte(byte (int (TagOps.tagType tag)))
        this.WriteTagPayload(tag)

    interface IDisposable with
        member _.Dispose() = stream.Dispose()

[<RequireQualifiedAccess>]
module Snbt =
    type private Parser(text: string) =
        let mutable index = 0

        member private _.Peek() =
            if index < text.Length then Some text[index] else None

        member private _.Read() =
            let c = text[index]
            index <- index + 1
            c

        member private this.SkipWhitespace() =
            while this.Peek() |> Option.exists Char.IsWhiteSpace do
                ignore (this.Read())

        member private this.Expect(ch: char) =
            this.SkipWhitespace()
            match this.Peek() with
            | Some actual when actual = ch -> ignore (this.Read())
            | _ -> raise (Errors.decode $"Expected '{ch}' in SNBT.")

        member private this.ParseQuotedString() =
            this.SkipWhitespace()
            let quote =
                match this.Peek() with
                | Some ('"' | '\'' as q) ->
                    ignore (this.Read())
                    q
                | _ -> raise (Errors.decode "Expected quoted SNBT string.")

            let builder = StringBuilder()
            let mutable closed = false

            let readHexDigits count =
                let hex = StringBuilder()

                for _ in 1 .. count do
                    match this.Peek() with
                    | Some c when Uri.IsHexDigit c ->
                        ignore (this.Read())
                        hex.Append(c) |> ignore
                    | _ -> raise (Errors.decode $"Expected {count} hex digits in SNBT escape.")

                hex.ToString()

            while not closed do
                match this.Peek() with
                | None -> raise (Errors.decode "Unterminated SNBT string.")
                | Some c when c = quote ->
                    ignore (this.Read())
                    closed <- true
                | Some '\\' ->
                    ignore (this.Read())
                    match this.Peek() with
                    | Some '\\' ->
                        ignore (this.Read())
                        builder.Append('\\') |> ignore
                    | Some '"' ->
                        ignore (this.Read())
                        builder.Append('"') |> ignore
                    | Some '\'' ->
                        ignore (this.Read())
                        builder.Append('\'') |> ignore
                    | Some 'b' ->
                        ignore (this.Read())
                        builder.Append('\b') |> ignore
                    | Some 's' ->
                        ignore (this.Read())
                        builder.Append(' ') |> ignore
                    | Some 't' ->
                        ignore (this.Read())
                        builder.Append('\t') |> ignore
                    | Some 'n' ->
                        ignore (this.Read())
                        builder.Append('\n') |> ignore
                    | Some 'f' ->
                        ignore (this.Read())
                        builder.Append('\u000C') |> ignore
                    | Some 'r' ->
                        ignore (this.Read())
                        builder.Append('\r') |> ignore
                    | Some 'x' ->
                        ignore (this.Read())
                        builder.Append(char (Convert.ToInt32(readHexDigits 2, 16))) |> ignore
                    | Some 'u' ->
                        ignore (this.Read())
                        builder.Append(char (Convert.ToInt32(readHexDigits 4, 16))) |> ignore
                    | Some 'U' ->
                        ignore (this.Read())
                        let codepoint = Convert.ToInt32(readHexDigits 8, 16)
                        builder.Append(String(Char.ConvertFromUtf32(codepoint))) |> ignore
                    | Some escaped ->
                        ignore (this.Read())
                        builder.Append(escaped) |> ignore
                    | None -> raise (Errors.decode "Invalid escape at end of SNBT string.")
                | Some c ->
                    ignore (this.Read())
                    builder.Append(c) |> ignore

            builder.ToString()

        member private this.ParseBareToken() =
            this.SkipWhitespace()
            let builder = StringBuilder()
            let mutable keepReading = true

            while keepReading do
                match this.Peek() with
                | Some c when not (Char.IsWhiteSpace c || c = ',' || c = ']' || c = '}' || c = ':') ->
                    builder.Append(this.Read()) |> ignore
                | _ ->
                    keepReading <- false

            let token = builder.ToString()
            if String.IsNullOrEmpty token then raise (Errors.decode "Expected SNBT token.")
            token

        member private this.ParseRelaxedToken(terminators: Set<char>) =
            this.SkipWhitespace()
            let builder = StringBuilder()
            let mutable closingDelimiters: char list = []
            let mutable quote: char option = None
            let mutable escaped = false
            let mutable keepReading = true

            while keepReading do
                match this.Peek(), quote with
                | None, _ ->
                    keepReading <- false
                | Some c, Some activeQuote ->
                    ignore (this.Read())
                    builder.Append(c) |> ignore

                    if escaped then
                        escaped <- false
                    elif c = '\\' then
                        escaped <- true
                    elif c = activeQuote then
                        quote <- None
                | Some c, None when List.isEmpty closingDelimiters && terminators.Contains c ->
                    keepReading <- false
                | Some c, None ->
                    ignore (this.Read())
                    builder.Append(c) |> ignore

                    match c with
                    | '"' | '\'' -> quote <- Some c
                    | '{' -> closingDelimiters <- '}' :: closingDelimiters
                    | '[' -> closingDelimiters <- ']' :: closingDelimiters
                    | '}' | ']' ->
                        match closingDelimiters with
                        | expected :: rest when c = expected -> closingDelimiters <- rest
                        | _ -> ()
                    | _ -> ()

            let token = builder.ToString().TrimEnd()
            if String.IsNullOrEmpty token then raise (Errors.decode "Expected SNBT token.")
            token

        member private this.ParseKey() =
            this.SkipWhitespace()
            match this.Peek() with
            | Some ('"' | '\'') -> this.ParseQuotedString()
            | _ -> this.ParseRelaxedToken(Set.singleton ':')

        member private this.ParseNumberOrIdentifier(token: string) =
            let lower = token.ToLowerInvariant()

            let tryParse suffix parse =
                if lower.EndsWith(suffix, StringComparison.Ordinal) then
                    parse (token.Substring(0, token.Length - 1))
                else
                    None

            let tryParseSignedInteger (text: string) =
                let isNegative = text.StartsWith("-", StringComparison.Ordinal)
                let unsigned =
                    if text.StartsWith("-", StringComparison.Ordinal) || text.StartsWith("+", StringComparison.Ordinal) then
                        text.Substring(1)
                    else
                        text

                let parseRadix prefix radix =
                    if unsigned.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                        let digits = unsigned.Substring(2)
                        if String.IsNullOrEmpty digits then
                            None
                        else
                            try
                                let value = Convert.ToInt64(digits, int radix)
                                Some(if isNegative then -value else value)
                            with _ ->
                                None
                    else
                        None

                parseRadix "0x" 16
                |> Option.orElseWith (fun () -> parseRadix "0b" 2)
                |> Option.orElseWith (fun () ->
                    match Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                    | true, value -> Some value
                    | _ -> None)

            match lower with
            | "end" -> Tag.End
            | _ ->
                match tryParse "b" (fun s -> tryParseSignedInteger s |> Option.map sbyte |> Option.map Tag.Byte) with
                | Some value -> value
                | None ->
                    match tryParse "s" (fun s -> tryParseSignedInteger s |> Option.map int16 |> Option.map Tag.Short) with
                    | Some value -> value
                    | None ->
                        match tryParse "l" (fun s -> tryParseSignedInteger s |> Option.map Tag.Long) with
                        | Some value -> value
                        | None ->
                            match tryParse "f" (fun s -> match Single.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with true, v -> Some(Tag.Float v) | _ -> None) with
                            | Some value -> value
                            | None ->
                                match tryParse "d" (fun s -> match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with true, v -> Some(Tag.Double v) | _ -> None) with
                                | Some value -> value
                                | None ->
                                    match tryParseSignedInteger token with
                                    | Some value when value >= int64 Int32.MinValue && value <= int64 Int32.MaxValue -> Tag.Int(int32 value)
                                    | Some _ -> Tag.String token
                                    | None ->
                                        match Double.TryParse(token, NumberStyles.Float, CultureInfo.InvariantCulture) with
                                        | true, value -> Tag.Double value
                                        | _ -> Tag.String token

        member private this.ParseTypedArray(kind: char) =
            ignore (this.Read())
            this.Expect ';'
            this.SkipWhitespace()

            let rec gather acc =
                this.SkipWhitespace()
                match this.Peek() with
                | Some ']' ->
                    ignore (this.Read())
                    List.rev acc
                | _ ->
                    let value = this.ParseValue()
                    this.SkipWhitespace()
                    match this.Peek() with
                    | Some ',' ->
                        ignore (this.Read())
                        gather (value :: acc)
                    | Some ']' ->
                        ignore (this.Read())
                        List.rev (value :: acc)
                    | _ -> raise (Errors.decode "Expected ',' or ']' in typed SNBT array.")

            let values = gather []
            match kind with
            | 'B' ->
                values
                |> List.map (function Tag.Byte v -> byte v | Tag.Int v -> byte v | x -> raise (Errors.decode $"Invalid byte array element: %A{x}"))
                |> List.toArray
                |> Tag.ByteArray
            | 'I' ->
                values
                |> List.map (function Tag.Int v -> v | x -> raise (Errors.decode $"Invalid int array element: %A{x}"))
                |> List.toArray
                |> Tag.IntArray
            | 'L' ->
                values
                |> List.map (function Tag.Long v -> v | Tag.Int v -> int64 v | x -> raise (Errors.decode $"Invalid long array element: %A{x}"))
                |> List.toArray
                |> Tag.LongArray
            | _ -> raise (Errors.decode $"Unsupported typed SNBT array kind '{kind}'.")

        member private this.ParseList() =
            this.Expect '['
            this.SkipWhitespace()

            match this.Peek(), if index + 1 < text.Length then Some text[index + 1] else None with
            | Some ('B' | 'I' | 'L' as kind), Some ';' -> this.ParseTypedArray(kind)
            | _ ->
                let rec gather acc =
                    this.SkipWhitespace()
                    match this.Peek() with
                    | Some ']' ->
                        ignore (this.Read())
                        List.rev acc
                    | _ ->
                        let value = this.ParseValue()
                        this.SkipWhitespace()
                        match this.Peek() with
                        | Some ',' ->
                            ignore (this.Read())
                            gather (value :: acc)
                        | Some ']' ->
                            ignore (this.Read())
                            List.rev (value :: acc)
                        | _ -> raise (Errors.decode "Expected ',' or ']' in SNBT list.")

                gather [] |> TagOps.listOf

        member private this.ParseCompound() =
            this.Expect '{'
            this.SkipWhitespace()

            let rec gather acc =
                this.SkipWhitespace()
                match this.Peek() with
                | Some '}' ->
                    ignore (this.Read())
                    List.rev acc
                | _ ->
                    let key = this.ParseKey()
                    this.Expect ':'
                    let value = this.ParseValue()
                    this.SkipWhitespace()
                    match this.Peek() with
                    | Some ',' ->
                        ignore (this.Read())
                        gather ((key, value) :: acc)
                    | Some '}' ->
                        ignore (this.Read())
                        List.rev ((key, value) :: acc)
                    | _ -> raise (Errors.decode "Expected ',' or '}' in SNBT compound.")

            gather [] |> Tag.Compound

        member private this.ParseValue() =
            this.SkipWhitespace()
            match this.Peek() with
            | Some '{' -> this.ParseCompound()
            | Some '[' -> this.ParseList()
            | Some ('"' | '\'') -> this.ParseQuotedString() |> Tag.String
            | Some _ -> this.ParseRelaxedToken(Set.ofList [ ','; ']'; '}' ]) |> this.ParseNumberOrIdentifier
            | None -> raise (Errors.decode "Unexpected end of SNBT input.")

        member this.Parse() =
            let value = this.ParseValue()
            this.SkipWhitespace()
            if index <> text.Length then
                raise (Errors.decode "Trailing characters found after SNBT value.")
            value

    let parse (text: string) =
        Parser(text).Parse()

[<RequireQualifiedAccess>]
module Serialization =
    let private withOutputStream compression (stream: MemoryStream) (action: Stream -> unit) =
        match compression with
        | Compression.None -> action stream
        | Compression.GZip ->
            use compressed = new GZipStream(stream, CompressionMode.Compress, true)
            action (compressed :> Stream)
        | Compression.ZLib ->
            use compressed = new ZLibStream(stream, CompressionMode.Compress, true)
            action (compressed :> Stream)

    let private detectCompression (data: byte[]) =
        if data.Length >= 2 && data[0] = 0x1Fuy && data[1] = 0x8Buy then
            Compression.GZip
        elif
            data.Length >= 2
            && data[0] = 0x78uy
            && (data[1] = 0x01uy || data[1] = 0x5Euy || data[1] = 0x9Cuy || data[1] = 0xDAuy)
        then
            Compression.ZLib
        else
            Compression.None

    let private openInputStream compression (data: byte[]) =
        let stream = new MemoryStream(data, writable = false)

        match compression with
        | Compression.None -> stream :> Stream
        | Compression.GZip -> new GZipStream(stream, CompressionMode.Decompress) :> Stream
        | Compression.ZLib -> new ZLibStream(stream, CompressionMode.Decompress) :> Stream

    let private ensureFullyConsumed (stream: Stream) =
        if stream.ReadByte() <> -1 then
            raise (Errors.decode "Trailing bytes found after decoding NBT payload.")

    let private tryReadNamed format compression policy data =
        try
            use stream = openInputStream compression data
            use reader = new NBTInputStream(stream, format = format, policy = policy)
            let tag = (reader.ReadNamedTag()).Tag
            ensureFullyConsumed stream
            Some tag
        with
        | :? NbtPolicyException as ex -> raise ex
        | _ ->
            None

    let private tryReadRaw format policy data =
        try
            use stream = new MemoryStream(data, writable = false)
            use reader = new NBTInputStream(stream, format = format, policy = policy)
            let tag = reader.ReadRawTag()
            ensureFullyConsumed stream
            Some tag
        with
        | :? NbtPolicyException as ex -> raise ex
        | _ ->
            None

    let toByteArrayWithFormat format name tag =
        use stream = new MemoryStream()
        use writer = new NBTOutputStream(stream, format = format)
        writer.WriteNamedTag(name, tag)
        stream.ToArray()

    let toByteArrayWith byteOrder name tag =
        toByteArrayWithFormat (NbtFormat.ofByteOrder byteOrder) name tag

    let toByteArray name tag = toByteArrayWithFormat NbtFormat.BigEndian name tag

    let toCompressedByteArrayWithFormat format compression name tag =
        use stream = new MemoryStream()

        withOutputStream compression stream (fun target ->
            use writer = new NBTOutputStream(target, format = format)
            writer.WriteNamedTag(name, tag))

        stream.ToArray()

    let toCompressedByteArrayWith byteOrder compression name tag =
        toCompressedByteArrayWithFormat (NbtFormat.ofByteOrder byteOrder) compression name tag

    let toCompressedByteArray compression name tag =
        toCompressedByteArrayWithFormat NbtFormat.BigEndian compression name tag

    let toRawByteArrayWithFormat format tag =
        use stream = new MemoryStream()
        use writer = new NBTOutputStream(stream, format = format)
        writer.WriteRawTag(tag)
        stream.ToArray()

    let toRawByteArrayWith byteOrder tag =
        toRawByteArrayWithFormat (NbtFormat.ofByteOrder byteOrder) tag

    let toRawByteArray tag = toRawByteArrayWithFormat NbtFormat.BigEndian tag

    let fromByteArrayWithFormatAndPolicy format policy (data: byte[]) =
        match tryReadNamed format Compression.None policy data with
        | Some tag -> tag
        | None -> raise (Errors.unsupported $"Unable to decode named NBT as {format}.")

    let fromByteArrayWithFormat format (data: byte[]) =
        fromByteArrayWithFormatAndPolicy format ReaderPolicy.Default data

    let fromByteArrayWithAndPolicy byteOrder policy (data: byte[]) =
        fromByteArrayWithFormatAndPolicy (NbtFormat.ofByteOrder byteOrder) policy data

    let fromByteArrayWith byteOrder (data: byte[]) =
        fromByteArrayWithAndPolicy byteOrder ReaderPolicy.Default data

    let fromByteArray (data: byte[]) = fromByteArrayWithFormat NbtFormat.BigEndian data

    let fromCompressedByteArrayWithFormatAndPolicy format compression policy (data: byte[]) =
        match tryReadNamed format compression policy data with
        | Some tag -> tag
        | None -> raise (Errors.unsupported $"Unable to decode named NBT as {format} with {compression} compression.")

    let fromCompressedByteArrayWithFormat format compression (data: byte[]) =
        fromCompressedByteArrayWithFormatAndPolicy format compression ReaderPolicy.Default data

    let fromCompressedByteArrayWithAndPolicy byteOrder compression policy (data: byte[]) =
        fromCompressedByteArrayWithFormatAndPolicy (NbtFormat.ofByteOrder byteOrder) compression policy data

    let fromCompressedByteArrayWith byteOrder compression (data: byte[]) =
        fromCompressedByteArrayWithAndPolicy byteOrder compression ReaderPolicy.Default data

    let fromCompressedByteArray compression (data: byte[]) =
        fromCompressedByteArrayWithFormat NbtFormat.BigEndian compression data

    let fromByteArrayAutoWithPolicy policy (data: byte[]) =
        let preferredCompression = detectCompression data

        let compressionCandidates =
            [ preferredCompression
              Compression.None
              Compression.GZip
              Compression.ZLib ]
            |> List.distinct

        let formatCandidates =
            [ NbtFormat.BigEndian
              NbtFormat.LittleEndian
              NbtFormat.LittleEndianVarInt ]

        compressionCandidates
        |> List.tryPick (fun compression ->
            formatCandidates
            |> List.tryPick (fun format -> tryReadNamed format compression policy data))
        |> function
            | Some tag -> tag
            | None -> raise (Errors.unsupported "Unable to auto-detect a supported named NBT encoding.")

    let fromByteArrayAuto (data: byte[]) =
        fromByteArrayAutoWithPolicy ReaderPolicy.Default data

    let fromRawByteArrayWithFormatAndPolicy format policy (data: byte[]) =
        match tryReadRaw format policy data with
        | Some tag -> tag
        | None -> raise (Errors.unsupported $"Unable to decode raw NBT as {format}.")

    let fromRawByteArrayWithFormat format (data: byte[]) =
        fromRawByteArrayWithFormatAndPolicy format ReaderPolicy.Default data

    let fromRawByteArrayWithAndPolicy byteOrder policy (data: byte[]) =
        fromRawByteArrayWithFormatAndPolicy (NbtFormat.ofByteOrder byteOrder) policy data

    let fromRawByteArrayWith byteOrder (data: byte[]) =
        fromRawByteArrayWithAndPolicy byteOrder ReaderPolicy.Default data

    let fromRawByteArray (data: byte[]) = fromRawByteArrayWithFormat NbtFormat.BigEndian data

    let fromRawByteArrayAutoWithPolicy policy (data: byte[]) =
        [ NbtFormat.BigEndian
          NbtFormat.LittleEndian
          NbtFormat.LittleEndianVarInt ]
        |> List.tryPick (fun format -> tryReadRaw format policy data)
        |> function
            | Some tag -> tag
            | None -> raise (Errors.unsupported "Unable to auto-detect a supported raw NBT encoding.")

    let fromRawByteArrayAuto (data: byte[]) =
        fromRawByteArrayAutoWithPolicy ReaderPolicy.Default data
