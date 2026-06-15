namespace VideoEdit

open System
open System.IO
open System.Diagnostics
open Renderer

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
