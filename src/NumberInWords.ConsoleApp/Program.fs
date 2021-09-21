open System
open Result
open GGroupp.NumberInWords


let private firstOrFailure collection =
    if Seq.length collection > 0 then Seq.item 0 collection |> Success
    else Failure "Аргументы командной строки отсутствуют"

let private parseOrFailure (text: string) =
    match Int64.TryParse text with
    | true, value -> Success value
    | _ -> Failure $"Значение не удалось распознать как Int64: {text}"

let private withCapitalLetter text =
    let toUpperOnlyFirst index symbol =
        match index with
        | 0 -> symbol |> Char.ToUpperInvariant
        | _ -> symbol

    text
    |> Seq.mapi toUpperOnlyFirst
    |> String.Concat

let private printText text =
    printfn "%O" text

let private finish _ =
    0

[<EntryPoint>]
let main argv =
    argv
    |> firstOrFailure
    |> forward parseOrFailure
    |> mapSuccess toWordsInRussian
    |> mapSuccess withCapitalLetter
    |> fold printText printText
    |> finish