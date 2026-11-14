namespace Rbt.VideoEdit

open System
open System.IO
open System.IO.Compression
open Rbt.Renderer

/// A small PNG writer for frames (stills, contact sheets, previews): 8-bit RGBA,
/// no filtering, zlib from the BCL. No ffmpeg needed.
module Png =

    let private crcTable =
        Array.init 256 (fun n ->
            let mutable c = uint32 n
            for _ in 0 .. 7 do c <- if c &&& 1u <> 0u then 0xEDB88320u ^^^ (c >>> 1) else c >>> 1
            c)

    let private crc (data: byte[]) (offset: int) (count: int) =
        let mutable c = 0xFFFFFFFFu
        for i in offset .. offset + count - 1 do
            c <- crcTable.[int ((c ^^^ uint32 data.[i]) &&& 0xFFu)] ^^^ (c >>> 8)
        c ^^^ 0xFFFFFFFFu

    let private be (v: uint32) = [| byte (v >>> 24); byte (v >>> 16); byte (v >>> 8); byte v |]

    let private chunk (out: Stream) (kind: string) (data: byte[]) =
        out.Write(be (uint32 data.Length), 0, 4)
        let body = Array.append (Text.Encoding.ASCII.GetBytes kind) data
        out.Write(body, 0, body.Length)
        out.Write(be (crc body 0 body.Length), 0, 4)

    /// Encode a framebuffer (BGRA in memory, as the renderer and the encoder use it) as PNG bytes.
    let encode (fb: Framebuffer) : byte[] =
        let w, h = fb.W, fb.H
        let px = fb.Px.Span
        let raw = Array.zeroCreate<byte> (h * (1 + w * 4))
        let mutable o = 0
        for y in 0 .. h - 1 do
            raw.[o] <- 0uy                       // filter: none
            o <- o + 1
            for x in 0 .. w - 1 do
                let v = px.[y * w + x]
                raw.[o] <- byte (v >>> 16)       // R
                raw.[o + 1] <- byte (v >>> 8)    // G
                raw.[o + 2] <- byte v            // B
                raw.[o + 3] <- byte (v >>> 24)   // A
                o <- o + 4
        use idat = new MemoryStream()
        do
            use z = new ZLibStream(idat, CompressionLevel.Fastest, true)
            z.Write(raw, 0, raw.Length)
        use out = new MemoryStream()
        out.Write([| 137uy; 80uy; 78uy; 71uy; 13uy; 10uy; 26uy; 10uy |], 0, 8)
        chunk out "IHDR" (Array.concat [ be (uint32 w); be (uint32 h); [| 8uy; 6uy; 0uy; 0uy; 0uy |] ])
        chunk out "IDAT" (idat.ToArray())
        chunk out "IEND" [||]
        out.ToArray()

    let save (path: string) (fb: Framebuffer) = File.WriteAllBytes(path, encode fb)
