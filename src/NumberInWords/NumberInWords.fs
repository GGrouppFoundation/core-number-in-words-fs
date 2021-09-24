namespace GGroupp

open InternalNumberInWords

module NumberInWords =

    [<Literal>]
    let private Zero = 0L

    [<Literal>]
    let private MinusOne = -1L

    [<Literal>]
    let private Empty = ""

    let private defaultDimension = {
            Nominative = Empty;
            GenitiveSingular = Empty;
            GenitivePlural = Empty;
            Gender = Masculine
        }

    let private writeMinus source = "минус " + source

    let toWordsInRussianWithDimension value dimension =
        if value >= Zero then
            (value |> uint64, dimension)
            |> internalGetRussianWords
        else
            (value * MinusOne |> uint64, dimension)
            |> internalGetRussianWords
            |> writeMinus

    let toWordsInRussian value =
        toWordsInRussianWithDimension value defaultDimension
