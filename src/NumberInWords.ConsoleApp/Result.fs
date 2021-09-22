module Result

open System.Runtime.CompilerServices


[<IsReadOnly; Struct>]
type internal Result<'success, 'failure> =
    | Success of success: 'success
    | Failure of failure: 'failure

let internal map mapSuccessFunc mapFailureFunc source =
    match source with
    | Success success -> mapSuccessFunc success |> Success
    | Failure failure -> mapFailureFunc failure |> Failure

let internal mapSuccess mapSuccessFunc source =
    match source with
    | Success success -> mapSuccessFunc success |> Success
    | Failure failure -> failure |> Failure

let internal forward forwardFunc source =
    match source with
    | Success success -> forwardFunc success
    | Failure failure -> failure |> Failure

let internal fold mapSuccessFunc mapFailureFunc result =
    match result with
    | Success success -> mapSuccessFunc success
    | Failure failure -> mapFailureFunc failure