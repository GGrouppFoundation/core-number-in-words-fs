namespace GGroupp

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type RussianWord = {
        Nominative: string
        GenitiveSingular: string
        GenitivePlural: string
        Gender: RussianWordGender
}