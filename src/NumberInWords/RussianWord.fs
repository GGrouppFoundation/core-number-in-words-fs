namespace GGroupp

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type RussianWord = {
        nominative: string
        genitiveSingular: string
        genitivePlural: string
        gender: RussianWordGender
    }