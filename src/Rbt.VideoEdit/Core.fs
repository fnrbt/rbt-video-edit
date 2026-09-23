namespace Rbt.VideoEdit

open Rbt.Renderer

/// Shared foundations: the time unit, frame geometry, the serializable Param type,
/// the generic open Registry, and small transform helpers.
[<AutoOpen>]
module Core =

    /// Time, in seconds.
    [<Measure>] type s

    /// The output frame geometry, in pixels. Threaded through evaluation so full-frame
    /// backgrounds and centered graphics know how big the canvas is.
    type Frame = { W: float; H: float }

    /// A serializable parameter value for registry-resolved effects/transitions/easings.
    type Param =
        | PFloat of float
        | PInt of int
        | PBool of bool
        | PColor of Color
        | PStr of string
        | PList of Param list

    type Params = Map<string, Param>

    let pmap (xs: (string * Param) list) : Params = Map.ofList xs

    let pfloat (ps: Params) (key: string) (def: float) =
        match Map.tryFind key ps with
        | Some (PFloat v) -> v
        | Some (PInt v) -> float v
        | _ -> def

    let pint (ps: Params) (key: string) (def: int) =
        match Map.tryFind key ps with
        | Some (PInt v) -> v
        | Some (PFloat v) -> int v
        | _ -> def

    let pbool (ps: Params) (key: string) (def: bool) =
        match Map.tryFind key ps with Some (PBool v) -> v | _ -> def

    let pstr (ps: Params) (key: string) (def: string) =
        match Map.tryFind key ps with Some (PStr v) -> v | _ -> def

    let pcolor (ps: Params) (key: string) (def: Color) =
        match Map.tryFind key ps with Some (PColor v) -> v | _ -> def

    let plist (xs: Param list) : Param = PList xs

    /// Read a list of strings from a PList param.
    let pstrs (ps: Params) (key: string) (def: string list) =
        match Map.tryFind key ps with
        | Some (PList xs) -> xs |> List.choose (function PStr s -> Some s | _ -> None)
        | _ -> def

    /// Human-readable rendering of a Param (for diffs / inspectors).
    let rec showParam (p: Param) : string =
        match p with
        | PFloat f -> sprintf "%.4g" f
        | PInt i -> string i
        | PBool b -> string b
        | PStr s -> "\"" + s + "\""
        | PColor c -> sprintf "#%02X%02X%02X" c.R c.G c.B
        | PList xs -> "[" + String.concat ", " (List.map showParam xs) + "]"

    /// A tiny open registry: maps a string name to an implementation. The basis of
    /// dynamic, extensible effect / transition / easing identities.
    type Registry<'T>() =
        let table = System.Collections.Generic.Dictionary<string, 'T>()
        member _.Register(name: string, impl: 'T) = table.[name] <- impl
        member _.TryGet(name: string) =
            match table.TryGetValue name with
            | true, v -> Some v
            | _ -> None
        member _.Names = table.Keys |> Seq.toList

    /// Compose a 2D transform `m` so that it is applied about the pivot point (px, py)
    /// rather than the origin — used for scale/rotate-about-center animations.
    let aboutPivot (px: float) (py: float) (m: Matrix3x2) : Matrix3x2 =
        Matrix3x2.multiply
            (Matrix3x2.multiply (Matrix3x2.translate -px -py) m)
            (Matrix3x2.translate px py)

    /// How a source image maps onto the frame.
    type Fit = Cover | Contain | Stretch

    /// Destination rect for fitting a srcW×srcH image into the frame.
    let fitRect (frame: Frame) (srcW: float) (srcH: float) (fit: Fit) : Rect =
        match fit with
        | Stretch -> rect 0.0 0.0 frame.W frame.H
        | _ ->
            let pick = if fit = Cover then max else min
            let scl = pick (frame.W / srcW) (frame.H / srcH)
            let w = srcW * scl
            let h = srcH * scl
            rect ((frame.W - w) / 2.0) ((frame.H - h) / 2.0) w h

    /// A closed rectangle as a path (for gradient fills and clip reveals).
    let rectPath (r: Rect) : PathSeg list =
        let x0 = float r.Pos.X
        let y0 = float r.Pos.Y
        let x1 = x0 + float r.Size.X
        let y1 = y0 + float r.Size.Y
        [ MoveTo (vec2 x0 y0); LineTo (vec2 x1 y0); LineTo (vec2 x1 y1); LineTo (vec2 x0 y1); Close ]

    /// Wrap vector content in a 4× antialias pass. Used by overlays/components; footage and
    /// flat backgrounds skip it (cheaper, and they don't need it).
    let aa (cmds: Cmd list) : Cmd list =
        match cmds with [] -> [] | _ -> [ AntiAlias(4, cmds) ]
