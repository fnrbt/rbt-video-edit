module Rbt.VideoEdit.Sample.Program

open Rbt.VideoEdit

// Dispatcher: `dotnet run -- <scene> [<scene> ...]` renders the named scenes to out/.
// With no args, lists the available scenes and the Fx catalog.
[<EntryPoint>]
let main argv =
    Theme.useBoldFont Theme.defaultBoldFontPath
    let scenes = Map.ofList Scenes.all
    if argv.Length = 0 then
        let fx, tr = Fx.catalog ()
        printfn "scenes: %s" (String.concat ", " (scenes |> Map.toList |> List.map fst))
        printfn "effects (%d): %s" (List.length fx) (String.concat ", " (List.sort fx))
        printfn "transitions (%d): %s" (List.length tr) (String.concat ", " (List.sort tr))
    elif argv.[0] = "sexp" then
        for path in argv.[1..] do Sexp.renderFile path
    else
        for n in argv do
            match Map.tryFind n scenes with
            | Some f -> printfn "=== rendering %s ===" n; f ()
            | None -> printfn "unknown scene: %s" n
    0
