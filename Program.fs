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
    ChunkId: byte []         // 4
    ChunkDataSize: uint32
}

type FormatChunk = {
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

type SampleLoop = {
    CuePointId: uint32
    Type: uint32
    Start: uint32
    End: uint32
    Fraction: uint32
    PlayCount: uint32
}

type SamplerChunk = {
    Manufacturer: byte []    // 4
    Product: byte []         // 4
    SamplePeriod: uint32
    MidiUnityNote: uint32
    MidiPitchFraction: uint32
    SmpteFormat: uint32
    SmpteOffset: uint32
    NumSampleLoops: uint32
    SamplerDataSize: uint32
    SampleLoops: SampleLoop []
    SamplerData: byte []
}

type ChunkData =
    | FormatChunk of FormatChunk
    | DataChunk of DataChunk
    | SamplerChunk of SamplerChunk
    | GenericChunk of byte []

type Chunk = {
    ChunkHeader: ChunkHeader
    Data: ChunkData
}

let readChunkHeader (rd: BinaryReader) =
    let hdr = {
        ChunkId = rd.ReadBytes(4)
        ChunkDataSize = rd.ReadUInt32()
    }
    // printfn "did read %d" (hdr.ChunkDataSize |> int)
    hdr

let writeChunkHeader (wr: BinaryWriter) (hdr: ChunkHeader) =
    wr.Write(hdr.ChunkId)
    // failwith (sprintf "%d" (hdr.ChunkDataSize |> int))
    wr.Write(hdr.ChunkDataSize)

type WavHeader = {
    ChunkHeader: ChunkHeader
    RiffType: byte []        // 4
}

type Wav = {
    Header: WavHeader
    Chunks: Chunk []
}

let readWavHeader (rd: BinaryReader) =
    let wavChnkHdr = readChunkHeader rd
    {
        ChunkHeader = wavChnkHdr
        RiffType = rd.ReadBytes(4)
    }

let writeWavHeader (wr: BinaryWriter) (hdr: WavHeader) =
    writeChunkHeader wr hdr.ChunkHeader
    wr.Write(hdr.RiffType)

let readFormatChunk (rd: BinaryReader) =
    let compCode = rd.ReadUInt16()
    assert (compCode = 1us)
    let noChans = rd.ReadUInt16()
    let sr = rd.ReadUInt32()
    let bps = rd.ReadUInt32()
    let blockAlign = rd.ReadUInt16()
    assert (blockAlign = 2us)
    let sigBits = rd.ReadUInt16()
    assert (sigBits = 16us)
    {
        CompressionCode = compCode
        NumberOfChannels = noChans
        SampleRate = sr
        AvgBytesPerSec = bps
        BlockAlign = blockAlign
        SigBitsPerSample = sigBits
    }

let writeFormatChunkData (wr: BinaryWriter) (f: FormatChunk) =
    wr.Write(f.CompressionCode)
    wr.Write(f.NumberOfChannels)
    wr.Write(f.SampleRate)
    wr.Write(f.AvgBytesPerSec)
    wr.Write(f.BlockAlign)
    wr.Write(f.SigBitsPerSample)

let readDataChunk (rd: BinaryReader) =
    let chunkSize = rd.ReadUInt32()
    {
        ChunkSize = chunkSize
        SampleData = rd.ReadBytes(chunkSize |> int)
    }

let writeDataChunkData (wr: BinaryWriter) (d: DataChunk) =
    wr.Write(d.ChunkSize)
    wr.Write(d.SampleData)

let rec readSampleLoops (rd: BinaryReader) (n: uint32) =
    match n with
    | 0u -> []
    | _ ->
        (
            {
                CuePointId = rd.ReadUInt32()
                Type = rd.ReadUInt32()
                Start = rd.ReadUInt32()
                End = rd.ReadUInt32()
                Fraction = rd.ReadUInt32()
                PlayCount = rd.ReadUInt32()
            } :: (readSampleLoops rd (n - 1u))
        )

let writeSampleLoops (wr: BinaryWriter) (loops: SampleLoop []) =
    let wrLoop loop =
        wr.Write(loop.CuePointId)
        wr.Write(loop.Type)
        wr.Write(loop.Start)
        wr.Write(loop.End)
        wr.Write(loop.Fraction)
        wr.Write(loop.PlayCount)
    loops |> Array.iter wrLoop

let readSamplerChunk (rd: BinaryReader) =
    let manufacturer = rd.ReadBytes(4)
    let product = rd.ReadBytes(4)
    let samplePeriod = rd.ReadUInt32()
    let midiUnityNote = rd.ReadUInt32()
    let midiPitchFraction = rd.ReadUInt32()
    let smpteFormat = rd.ReadUInt32()
    let smpteOffset = rd.ReadUInt32()
    let numSampleLoops = rd.ReadUInt32()
    let samplerDataSize = rd.ReadUInt32()
    let sampleLoops = readSampleLoops rd numSampleLoops |> List.toArray
    {
        Manufacturer = manufacturer
        Product = product
        SamplePeriod = samplePeriod
        MidiUnityNote = midiUnityNote
        MidiPitchFraction = midiPitchFraction
        SmpteFormat = smpteFormat
        SmpteOffset = smpteOffset
        NumSampleLoops = numSampleLoops
        SamplerDataSize = samplerDataSize
        SampleLoops = sampleLoops
        SamplerData = rd.ReadBytes(samplerDataSize |> int)
    }

let writeSamplerChunkData (wr: BinaryWriter) (s: SamplerChunk) =
    wr.Write(s.Manufacturer)
    wr.Write(s.Product)
    wr.Write(s.SamplePeriod)
    wr.Write(s.MidiUnityNote)
    wr.Write(s.MidiPitchFraction)
    wr.Write(s.SmpteFormat)
    wr.Write(s.SmpteOffset)
    wr.Write(s.NumSampleLoops)
    wr.Write(s.SamplerDataSize)
    writeSampleLoops wr s.SampleLoops
    wr.Write(s.SamplerData)

let readChunkData (rd: BinaryReader) (hdr: ChunkHeader) =
    match hdr.ChunkId |> asciiString with
    | "fmt " -> FormatChunk (readFormatChunk rd)
    | "data" -> DataChunk (readDataChunk rd)
    | "smpl" -> SamplerChunk (readSamplerChunk rd)
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

let rec writeChunks (wr: BinaryWriter) (chunks: List<Chunk>) =
    match chunks with
    | [] -> ()
    | c::rest ->
        writeChunkHeader wr c.ChunkHeader
        match c.Data with
        | FormatChunk(f) -> writeFormatChunkData wr f
        | DataChunk(d) -> writeDataChunkData wr d
        | SamplerChunk(s) -> writeSamplerChunkData wr s
        | GenericChunk(g) -> wr.Write(g)
        writeChunks wr rest

let parseWavFile (wavFileName: string) =
    use rd = new BinaryReader(File.Open(wavFileName, FileMode.Open))
    let wavHdr = (readWavHeader rd)
    {
        Header = wavHdr
        Chunks = readChunks rd |> List.toArray
    }

let writeWavFile (wav: Wav) (outFileName: string) =
    use wr = new BinaryWriter(File.Open(outFileName, FileMode.Create))
    writeWavHeader wr wav.Header
    writeChunks wr (wav.Chunks |> Array.toList)

let isSamplesChunk (chunk: Chunk) =
    match chunk.Data with
    | DataChunk(_) -> true
    | _ -> false

let extractChunkSamples (chunk: Chunk) : int [] =
    let sampleBytes =
        match chunk.Data with
        | DataChunk(c) -> c.SampleData
        | _ -> [||]
    sampleBytes
        |> Array.chunkBySize 2
        |> Array.map (fun a ->
            ((uint16 a.[1]) <<< 8 ||| (uint16 a.[0])) |> int16 |> int)

let extractWavSamples (wav: Wav) : int [] [] =
    wav.Chunks
        |> Array.filter isSamplesChunk
        |> Array.map extractChunkSamples

let sampleRate (wav: Wav) =
    let sr (chunk: Chunk) =
        match chunk.Data with
        | FormatChunk(c) -> Some(c.SampleRate)
        | _ -> None
    let sampleRates =
        wav.Chunks
        |> Array.map sr
        |> Array.choose id
        |> Array.distinct
    assert (sampleRates.Length = 1)
    sampleRates.[0]

let hexDisplayShorts (data: uint16 []) : unit =
    data
        |> Array.map (sprintf "%04x")
        |> Array.chunkBySize 16
        |> Array.map (Array.toSeq >> String.concat " ")
        |> Array.toSeq
        |> String.concat "\n"
        |> printfn "%s"

let decDisplayShorts (data: int []): unit =
    let header = "sample"
    printfn "%s" header
    data
        |> Array.iter (int >> (printfn "%d"))

let findStartStop wav =
    let allSamples = wav |> extractWavSamples
    assert (allSamples.Length = 1)
    let samples = allSamples.[0]
    let winSize = (sampleRate wav) / 1000ul |> int
    let windows =
        samples.[0..(samples.Length - 1 - winSize)]
        |> Array.mapi (fun i _ -> samples.[i..(i + winSize)])
    let amps =
        windows
        |> Array.map ((fun win -> win |> Array.map System.Math.Abs) >> Array.sum)
    let findGoodAmp amps =
        let sortedAmps = amps |> Array.sort
        let iMedian = amps.Length / 2
        sortedAmps.[iMedian]
    let goodAmp = findGoodAmp amps
    let rec findStart iCurr iLimit =
        if amps.[iCurr] <= goodAmp || iCurr + 1 = iLimit then
            iCurr
        else
            findStart iCurr iLimit

    // an eighth is a guess as to the usual location for sustain level
    let start = findStart (amps.Length / 8) (3 * amps.Length / 5)
    let distance (x1: int []) (x2: int []) =
        Array.map2 (fun a b -> (abs (a - b))) x1 x2
        |> Array.sum
    let findStop last first =
        let indexedL1Distances =
            windows
            |> Array.indexed
            |> Array.filter (fun (i, _) -> i >= first && i <= last)
            |> Array.map (fun (i, win) -> (i, distance win windows.[start]))
        let folder (iCurr, curr) (iNext, next) =
            if next < curr then
                (iNext, next)
            else
                (iCurr, curr)
        let iMin =
            indexedL1Distances.[1..]
            |> Array.fold folder indexedL1Distances.[0]
            |> fst
        iMin
    let stop = findStop (amps.Length - (amps.Length / 8)) (amps.Length / 2)
    (start, stop)

let makeSampleLoop start stop =
    {
        CuePointId = 0xEDul
        Type = 0ul
        Start = start |> uint32
        End = stop |> uint32
        Fraction = 0ul
        PlayCount = 0ul
    }

let makeSamplerChunk (sr: int) (sampleLoop: SampleLoop) =
    {
        ChunkHeader = {
            ChunkId = "smpl" |> asciiBytes
            ChunkDataSize = 36ul + 24ul
        }
        Data = SamplerChunk {
            Manufacturer = "EdLC" |> asciiBytes
            Product = "L00p" |> asciiBytes
            SamplePeriod = 1_000_000_000.0 * (float sr) / 1.0 |> uint32
            MidiUnityNote = 60ul
            MidiPitchFraction = 0ul
            SmpteFormat = 0ul
            SmpteOffset = 0ul
            NumSampleLoops = 1ul
            SamplerDataSize = 0ul
            SampleLoops = [|sampleLoop|]
            SamplerData = "" |> asciiBytes
        }
    }


let addLoop (wav: Wav) start stop =
    let samplerChunkId = "smpl" |> asciiBytes
    let formatChunkId = "fmt " |> asciiBytes
    let samplerChunkIdx =
        wav.Chunks
        |> Array.indexed
        |> Array.filter (fun (_, c) -> c.ChunkHeader.ChunkId = samplerChunkId)
        |> Array.map fst
        |> Array.tryHead
    let sampleLoop = makeSampleLoop start stop
    let samplerChunk =
        match samplerChunkIdx with
        | Some(i) ->
            match wav.Chunks.[i].Data with
            | SamplerChunk(c) ->
                {
                    ChunkHeader = wav.Chunks.[i].ChunkHeader
                    Data = SamplerChunk { c with SampleLoops = [|sampleLoop|] }
                }
            | _ -> failwith "Sampler chunk isn't a sampler chunk"
        | None -> makeSamplerChunk (sampleRate wav |> int) sampleLoop
    let newChunks =
        match samplerChunkIdx with
        | Some(i) ->
            wav.Chunks
            |> Array.mapi (fun j c -> if j = i then samplerChunk else c)
        | None ->
            let fmtChunks =
                wav.Chunks
                |> Array.filter (fun c -> c.ChunkHeader.ChunkId = formatChunkId)
            let otherChunks =
                wav.Chunks
                |> Array.filter (fun c ->
                    let cId = c.ChunkHeader.ChunkId
                    cId <> formatChunkId && cId <> samplerChunkId)
            Array.append
                (Array.append fmtChunks [|samplerChunk|])
                otherChunks
    let newSize = wav.Header.ChunkHeader.ChunkDataSize + samplerChunk.ChunkHeader.ChunkDataSize
    let newWavChunkHdr = {
        wav.Header.ChunkHeader with ChunkDataSize = newSize
    }
    let newWavHeader = {
        wav.Header with ChunkHeader = newWavChunkHdr
    }
    {
        wav with Chunks = newChunks; Header = newWavHeader
    }

[<EntryPoint>]
let main argv =
    match argv with
    | [|wavFileName; "-j"|] ->
        let wav = parseWavFile wavFileName
        wav |> JsonConvert.SerializeObject |> printf "%s"
        0
    | [|wavFileName; "-s"|] ->
        let wav = parseWavFile wavFileName
        wav
            |> extractWavSamples
            |> Array.iter decDisplayShorts
        0
    | [|wavFileName|] ->
        let wav = parseWavFile wavFileName
        let (start, stop) = findStartStop wav
        printfn "start: %d" start
        printfn "stop: %d" stop
        0
    | [|inWavFileName; outWavFileName|] ->
        let inWav = parseWavFile inWavFileName
        let (start, stop) = findStartStop inWav
        let outWav = addLoop inWav start stop
        // outWav |> JsonConvert.SerializeObject |> printf "%s"
        writeWavFile outWav outWavFileName
        0
    | _ -> 1