// Thanks to
// https://sites.google.com/site/musicgapi/technical-documents/wav-file-format#wavefileheader
// for the format description.
open Newtonsoft.Json
open System.IO

let asciiString (bytes: byte []) =
    System.Text.Encoding.ASCII.GetString bytes

let asciiBytes (s: string) =
    System.Text.Encoding.ASCII.GetBytes s

type ChunkHeader = {
    ChunkId: string         // 4
    ChunkDataSize: uint32
}

type FmtChunk = {
    CompressionCode: uint16
    NumberOfChannels: uint16
    SampleRate: uint32
    AvgBytesPerSec: uint32
    BlockAlign: uint16
    SigBitsPerSample: uint16
    // Compression is not supported
    // NExtraFormatBytes: uint16
    // Extra: byte []
}

type DataChunk = {
    ChunkSize: uint32
    SampleData: byte []
}

type ChunkData =
    | FmtChunk of FmtChunk
    | DataChunk of DataChunk
    | GenericChunk of byte []

type Chunk = {
    ChunkHeader: ChunkHeader
    Data: ChunkData
}

let readChunkHeader (rd: BinaryReader) =
    {
        ChunkId = rd.ReadBytes(4) |> asciiString
        ChunkDataSize = rd.ReadUInt32()       
    }

type WavHeader = {
    ChunkHeader: ChunkHeader
    RiffType: string        // 4
}

type Wav = {
    Header: WavHeader
    Chunks: Chunk []
}

let readWavHeader (rd: BinaryReader) =
    let wavChnkHdr = readChunkHeader rd
    {
        ChunkHeader = wavChnkHdr
        RiffType = rd.ReadBytes(4) |> asciiString
    }

let readFmtChunk (rd: BinaryReader) =
    let compCode = rd.ReadUInt16()
    assert (compCode = 1us)
    {
        CompressionCode = compCode
        NumberOfChannels = rd.ReadUInt16()
        SampleRate = rd.ReadUInt32()
        AvgBytesPerSec = rd.ReadUInt32()
        BlockAlign = rd.ReadUInt16()
        SigBitsPerSample = rd.ReadUInt16()
    }

let readDataChunk (rd: BinaryReader) =
    let chunkSize = rd.ReadUInt32()
    {
        ChunkSize = chunkSize
        SampleData = rd.ReadBytes(chunkSize |> int)
    }

let readChunkData (rd: BinaryReader) (hdr: ChunkHeader) =
    match hdr.ChunkId with
    | "fmt " -> FmtChunk (readFmtChunk rd)
    | "data" -> DataChunk (readDataChunk rd)
    | _ -> GenericChunk (rd.ReadBytes(hdr.ChunkDataSize |> int))

let rec readChunks (rd: BinaryReader) =
    if rd.BaseStream.Position = rd.BaseStream.Length then
        []
    else
        let hdr = readChunkHeader rd
        {
            ChunkHeader = hdr
            Data = readChunkData rd hdr
        } :: (readChunks rd)

let parseWavFile (wavFileName: string) =
    use rd = new BinaryReader(File.Open(wavFileName, FileMode.Open))
    let wavHdr = (readWavHeader rd)
    {
        Header = wavHdr
        Chunks = readChunks rd |> List.toArray
    }

[<EntryPoint>]
let main argv =
    match argv with
    (*
    | [|pgmFileName; outPgmFileName|] ->
        let pgm = parsePgmFile pgmFileName
        writePgm pgm outPgmFileName
        0
    *)
    | [|wavFileName|] ->
        parseWavFile wavFileName |> JsonConvert.SerializeObject |> printf "%s"
        0
    | _ -> 1