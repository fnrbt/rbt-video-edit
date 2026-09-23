namespace VideoEdit

open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open Rbt.Renderer

type RenderConfig =
    { W: int
      H: int
      Fps: int
      Background: Color
      Output: string
      Crf: int
      Preset: string }

/// Sink for rendered frames. Slice 1 ships the ffmpeg-CLI implementation; an NVENC sink can
/// be swapped in later behind the same interface.
type Encoder =
    abstract WriteFrame : Framebuffer -> unit
    abstract Finish : unit -> unit

/// Pipes raw BGRA frames to the `ffmpeg` CLI over stdin and muxes H.264 into an .mp4. When
/// `audio` placements are supplied, their files are added as extra inputs and an audio
/// filtergraph (atrim/atempo/volume/afade/adelay per placement → amix) is mixed and muxed
/// alongside the video. (Framebuffer.Px is BGRA premultiplied uint32; opaque frames make
/// premul == straight, so no un-premultiply is needed given each frame starts with a Clear.)
type FfmpegCliEncoder(cfg: RenderConfig, audio: AudioPlacement list, totalDur: float<s>) =
    static let inv (x: float) = x.ToString(CultureInfo.InvariantCulture)

    let videoIn =
        [ "-y"; "-f"; "rawvideo"; "-pixel_format"; "bgra"
          "-video_size"; sprintf "%dx%d" cfg.W cfg.H
          "-framerate"; string cfg.Fps; "-i"; "pipe:0" ]
    let vcodec =
        [ "-c:v"; "libx264"; "-pix_fmt"; "yuv420p"
          "-crf"; string cfg.Crf; "-preset"; cfg.Preset ]

    let audioArgs () =
        // distinct files become inputs 1..N (pipe:0 is input 0)
        let paths = audio |> List.map (fun p -> p.Path) |> List.distinct
        let idxOf path = List.findIndex ((=) path) paths + 1
        let chain j (p: AudioPlacement) =
            let srcEnd = float p.In + float p.Dur * p.Speed
            let parts = ResizeArray<string>()
            parts.Add(sprintf "[%d:a]atrim=start=%s:end=%s,asetpts=PTS-STARTPTS,aresample=48000,aformat=sample_fmts=fltp:channel_layouts=stereo"
                          (idxOf p.Path) (inv (float p.In)) (inv srcEnd))
            if p.Speed <> 1.0 then parts.Add(sprintf "atempo=%s" (inv p.Speed))
            if p.Gain <> 1.0 then parts.Add(sprintf "volume=%s" (inv p.Gain))
            if float p.FadeIn > 0.0 then parts.Add(sprintf "afade=t=in:st=0:d=%s" (inv (float p.FadeIn)))
            if float p.FadeOut > 0.0 then
                parts.Add(sprintf "afade=t=out:st=%s:d=%s" (inv (max 0.0 (float p.Dur - float p.FadeOut))) (inv (float p.FadeOut)))
            parts.Add(sprintf "adelay=%d:all=1" (int (float p.Start * 1000.0)))
            sprintf "%s[a%d]" (String.concat "," parts) j
        let chains = audio |> List.mapi chain
        let labels = String.concat "" [ for j in 0 .. audio.Length - 1 -> sprintf "[a%d]" j ]
        let mix =
            sprintf "%samix=inputs=%d:normalize=0:dropout_transition=0[mix];[mix]atrim=end=%s[aout]"
                labels audio.Length (inv (float totalDur))
        let filter = String.concat ";" (chains @ [ mix ])
        (paths |> List.collect (fun p -> [ "-i"; p ]))
        @ [ "-filter_complex"; filter; "-map"; "0:v"; "-map"; "[aout]" ]
        @ vcodec @ [ "-c:a"; "aac"; "-b:a"; "192k" ]

    let args =
        videoIn
        @ (if List.isEmpty audio then vcodec else audioArgs ())
        @ [ "-movflags"; "+faststart"; cfg.Output ]

    let psi = ProcessStartInfo("ffmpeg")
    do
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true
        args |> List.iter psi.ArgumentList.Add
    let proc = Process.Start psi
    let stdin = proc.StandardInput.BaseStream
    let buffer : byte[] = Array.zeroCreate (cfg.W * cfg.H * 4)

    /// Back-compat: video-only encoder.
    new(cfg: RenderConfig) = FfmpegCliEncoder(cfg, [], 0.0<s>)

    interface Encoder with
        member _.WriteFrame(fb: Framebuffer) =
            let bytes = MemoryMarshal.AsBytes(fb.Px.Span)
            bytes.CopyTo(Span<byte>(buffer))
            stdin.Write(buffer, 0, buffer.Length)
        member _.Finish() =
            stdin.Flush()
            stdin.Close()
            proc.WaitForExit()
