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

type Chunk = {
    ChunkHeader: ChunkHeader
    Data: byte []
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

let rec readChunks (rd: BinaryReader) =
    if rd.BaseStream.Position = rd.BaseStream.Length then
        []
    else
        let hdr = readChunkHeader rd
        {
            ChunkHeader = hdr
            Data = rd.ReadBytes(hdr.ChunkDataSize |> int)
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