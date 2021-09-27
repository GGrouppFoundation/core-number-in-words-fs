namespace GGroupp

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type RussianWordGender =
    | Masculine
    | Feminine
    | Neuter