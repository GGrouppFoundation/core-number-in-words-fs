open System
open Result
open GGroupp.NumberInWords

let private firstArgumentOrFailure argv =
    if Seq.length argv > 0 then Seq.item 0 argv |> Success
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

let private printText text = printfn "%O" text

let private toSuccessCode _ = 0

let private toFailureCode _ = -1

[<EntryPoint>]
let main argv =
    argv
    |> firstArgumentOrFailure
    |> forward parseOrFailure
    |> mapSuccess toWordsInRussian
    |> mapSuccess withCapitalLetter
    |> map printText printText
    |> fold toSuccessCode toFailureCode
