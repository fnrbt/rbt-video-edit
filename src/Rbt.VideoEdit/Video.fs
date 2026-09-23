namespace Rbt.VideoEdit

open System
open System.IO
open System.Diagnostics
open Rbt.Renderer

/// Decoded video footage: BGRA frames at W×H, indexed by time. Decoding shells out to the
/// ffmpeg CLI (symmetric with the encoder) and cover-crops to exactly the requested size.
type VideoSource =
    { Frames: Framebuffer[]; Fps: float; W: int; H: int
      Path: string         // original file (so the clip's audio can be referenced + bound)
      Start: float<s> }    // source in-point used at decode (audio aligns to this)

module Video =

    let private decode (path: string) (startSec: float) (durSec: float option) (w: int) (h: int) (fps: int) : VideoSource =
        let psi = ProcessStartInfo("ffmpeg")
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        let args = ResizeArray<string>()
        args.AddRange [| "-v"; "error" |]
        if startSec > 0.0 then args.AddRange [| "-ss"; string startSec |]
        args.AddRange [| "-i"; path |]
        match durSec with Some d -> args.AddRange [| "-t"; string d |] | None -> ()
        args.AddRange
            [| "-vf"; sprintf "fps=%d,scale=%d:%d:force_original_aspect_ratio=increase,crop=%d:%d" fps w h w h
               "-f"; "rawvideo"; "-pix_fmt"; "bgra"; "pipe:1" |]
        args |> Seq.iter psi.ArgumentList.Add
        let p = Process.Start psi
        use ms = new MemoryStream()
        p.StandardOutput.BaseStream.CopyTo ms
        let err = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then failwithf "ffmpeg decode failed for %s: %s" path err
        let bytes = ms.ToArray()
        let frameBytes = w * h * 4
        let n = bytes.Length / frameBytes
        if n = 0 then failwithf "no frames decoded from %s" path
        let frames =
            Array.init n (fun i ->
                let arr = Array.zeroCreate<uint32> (w * h)
                Buffer.BlockCopy(bytes, i * frameBytes, arr, 0, frameBytes)
                { W = w; H = h; Px = Memory(arr) })
        { Frames = frames; Fps = float fps; W = w; H = h; Path = path; Start = startSec * 1.0<s> }

    /// Decode an entire file at w×h / fps.
    let load (path: string) (w: int) (h: int) (fps: int) : VideoSource = decode path 0.0 None w h fps

    /// Decode a [startSec, startSec+durSec) slice — cheaper, bounds memory.
    let loadClip (path: string) (startSec: float) (durSec: float) (w: int) (h: int) (fps: int) : VideoSource =
        decode path startSec (Some durSec) w h fps

    // Cache decoded sources by (path, range, size, fps) so re-builds / re-loads don't re-decode.
    let private cache = System.Collections.Generic.Dictionary<string, VideoSource>()
    let loadCached (path: string) (startSec: float) (durSec: float) (w: int) (h: int) (fps: int) : VideoSource =
        let key = sprintf "%s|%g|%g|%d|%d|%d" path startSec durSec w h fps
        match cache.TryGetValue key with
        | true, v -> v
        | _ -> let v = loadClip path startSec durSec w h fps in cache.[key] <- v; v

    let duration (src: VideoSource) : float<s> = float src.Frames.Length / src.Fps * 1.0<s>

    /// Sample the frame at local time t (clamped to the last decoded frame).
    let frameAt (src: VideoSource) (t: float<s>) : Framebuffer =
        let i = int (float t * src.Fps)
        src.Frames.[max 0 (min (src.Frames.Length - 1) i)]

    /// A STREAMING footage source: decodes frames from ffmpeg on demand into a bounded sliding
    /// window instead of loading the whole clip into RAM. Memory is ~windowSize×W×H×4 regardless
    /// of clip length, so 4K / minutes-long footage no longer costs gigabytes. Thread-safe (the
    /// parallel renderer fetches frames from multiple threads); assumes roughly-MONOTONIC access
    /// (a forward render) — a backward jump past the window triggers a re-seek (fine but slower),
    /// so prefer the in-RAM `loadCached` for heavy random access / freeze-frames on short clips.
    type StreamSource(path: string, startSec: float, durSec: float, w: int, h: int, fps: int) =
        let frameBytes = w * h * 4
        let gate = obj ()
        let window = System.Collections.Generic.Dictionary<int, Framebuffer>()
        let order = System.Collections.Generic.Queue<int>()
        let windowMax = 64
        let mutable proc : Process = null
        let mutable stream : IO.Stream = null
        let mutable decodePos = 0                 // next clip-frame index the stream will yield
        let mutable lastFb : Framebuffer = { W = w; H = h; Px = Memory(Array.zeroCreate<uint32> (w * h)) }
        let startAt (atFrame: int) =
            if not (isNull proc) then (try proc.Kill() with _ -> ()); proc.Dispose()
            let psi = ProcessStartInfo("ffmpeg")
            psi.UseShellExecute <- false; psi.RedirectStandardOutput <- true; psi.RedirectStandardError <- true
            let ss = startSec + float atFrame / float fps
            let args = ResizeArray<string>([ "-v"; "error" ])
            if ss > 0.0 then args.AddRange [| "-ss"; string ss |]
            args.AddRange [| "-i"; path
                             "-vf"; sprintf "fps=%d,scale=%d:%d:force_original_aspect_ratio=increase,crop=%d:%d" fps w h w h
                             "-f"; "rawvideo"; "-pix_fmt"; "bgra"; "pipe:1" |]
            args |> Seq.iter psi.ArgumentList.Add
            proc <- Process.Start psi
            stream <- proc.StandardOutput.BaseStream
            decodePos <- atFrame
        let readOne () : Framebuffer option =
            let buf = Array.zeroCreate<byte> frameBytes
            let mutable off = 0
            let mutable eof = false
            while not eof && off < frameBytes do
                let r = stream.Read(buf, off, frameBytes - off)
                if r <= 0 then eof <- true else off <- off + r
            if off < frameBytes then None
            else
                let arr = Array.zeroCreate<uint32> (w * h)
                Buffer.BlockCopy(buf, 0, arr, 0, frameBytes)
                Some { W = w; H = h; Px = Memory(arr) }
        member _.W = w
        member _.H = h
        member _.Path = path
        member _.Start = startSec * 1.0<s>
        member _.Duration : float<s> = durSec * 1.0<s>
        member _.FrameAt(t: float<s>) : Framebuffer =
            let i = max 0 (int (float t * float fps))
            lock gate (fun () ->
                match window.TryGetValue i with
                | true, fb -> fb
                | _ ->
                    if isNull proc || i < decodePos then startAt i      // first use, or a backward jump
                    let mutable result = lastFb
                    let mutable going = true
                    while going && decodePos <= i do
                        match readOne () with
                        | Some fb ->
                            window.[decodePos] <- fb; order.Enqueue decodePos
                            lastFb <- fb
                            if decodePos = i then result <- fb
                            decodePos <- decodePos + 1
                            while order.Count > windowMax do window.Remove(order.Dequeue()) |> ignore
                        | None -> going <- false                        // EOF → freeze on last frame
                    result)

    /// Open a streaming footage source for a [startSec, startSec+durSec) slice at w×h / fps.
    let stream (path: string) (startSec: float) (durSec: float) (w: int) (h: int) (fps: int) : StreamSource =
        StreamSource(path, startSec, durSec, w, h, fps)
