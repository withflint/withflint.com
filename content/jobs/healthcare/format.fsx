#r "nuget: fsharpplus, 1.2.2"

open System.IO
open FSharpPlus

let print a =
    printfn "%A" a
    a

let lines (path: string) =
    let content = seq { yield! File.ReadLines path } |> Seq.toList
    (path, content)

let root = Directory.GetCurrentDirectory()


let parseJob =
    function
    | (path, (_ :: _ :: url :: title :: loc :: salary :: full :: body)) ->
        let lowercaseurl =
            url + "-" + loc
            |> String.toLower
            |> String.replace " " "-"
            |> String.replace "/" "-"
            |> String.replace "," "-"
            |> String.replace "(" "-"
            |> String.replace ")" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"
            |> String.replace "--" "-"

        let content =
            lowercaseurl
            :: title :: loc :: salary :: full :: "" :: body
            |> String.concat "\n"

        print lowercaseurl |> ignore
        Some($"{root}/{lowercaseurl}.md", content)
    | _ -> None

Directory.GetFiles($"/Users/.../nul/jobs/", "xx*")
|> Seq.sortDescending
|> Seq.map (lines >> parseJob)
|> Seq.choose id
|> Seq.iter File.WriteAllText
