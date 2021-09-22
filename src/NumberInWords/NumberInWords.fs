namespace GGroupp

open InternalNumberInWords

module NumberInWords =
    [<Literal>]
    let private Zero = 0L

    [<Literal>]
    let private MinusOne = -1L

    [<Literal>]
    let private Empty = ""

    let private defaultDimension =
        {
            nominative = Empty;
            genitiveSingular = Empty;
            genitivePlural = Empty;
            gender = Masculine
        }

    let private writeMinus source = "минус " + source

    let toWordsInRussianWithDimension value dimension =
        if value >= Zero then
            (value |> uint64, dimension)
            |> internalToWordsInRussian
        else
            (value * MinusOne |> uint64, dimension)
            |> internalToWordsInRussian
            |> writeMinus

    let toWordsInRussian value =
        toWordsInRussianWithDimension value defaultDimension
