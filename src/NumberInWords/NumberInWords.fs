namespace GGroupp

open InternalNumberInWords

module NumberInWords =

    [<Literal>]
    let private Zero = 0L

    [<Literal>]
    let private MinusOne = -1L

    [<Literal>]
    let private Empty = ""

    [<Literal>]
    let private WhiteSpace = " "

    let private Minus = "минус"

    let private defaultDimension = {
        Nominative = Empty;
        GenitiveSingular = Empty;
        GenitivePlural = Empty;
        Gender = Masculine
    }

    let private join first next =
        seq {
            yield first
            yield! next
        }

    let toWordsInRussianWithDimension value dimension =
        if value >= Zero then
            (value |> uint64, dimension)
            |> internalGetRussianWords
        else
            (value * MinusOne |> uint64, dimension)
            |> internalGetRussianWords
            |> join Minus
        |> String.concat WhiteSpace

    let toWordsInRussian value =
        toWordsInRussianWithDimension value defaultDimension
