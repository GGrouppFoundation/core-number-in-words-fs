namespace GGroupp


module internal InternalNumberInWords =
    [<Literal>]
    let private ZeroUL = 0UL

    [<Literal>]
    let private Zero = 0

    [<Literal>]
    let private Ten = 10

    [<Literal>]
    let private Hundred = 100

    [<Literal>]
    let private ThousandUL = 1000UL

    [<Literal>]
    let private Empty = ""

    [<Literal>]
    let private WhiteSpace = " "
    
    let private getDimensionRussianWord position defaultDimension =
        match position with
        | 1 -> { nominative = "тысяча"; genitiveSingular = "тысячи"; genitivePlural = "тысяч"; gender = Feminine }
        | 2 -> { nominative = "миллион"; genitiveSingular = "миллиона"; genitivePlural = "миллионов"; gender = Masculine }
        | 3 -> { nominative = "миллиард"; genitiveSingular = "миллиарда"; genitivePlural = "миллиардов"; gender = Masculine }
        | 4 -> { nominative = "триллион"; genitiveSingular = "триллиона"; genitivePlural = "триллионов"; gender = Masculine }
        | 5 -> { nominative = "квадриллион"; genitiveSingular = "квадриллиона"; genitivePlural = "квадриллионов"; gender = Masculine }
        | 6 -> { nominative = "квинтиллион"; genitiveSingular = "квинтиллиона"; genitivePlural = "квинтиллионов"; gender = Masculine }
        | 7 -> { nominative = "секстиллион"; genitiveSingular = "секстиллиона"; genitivePlural = "секстиллионов"; gender = Masculine }
        | 8 -> { nominative = "септиллион"; genitiveSingular = "септиллиона"; genitivePlural = "септиллионов"; gender = Masculine }
        | 9 -> { nominative = "октиллион"; genitiveSingular = "октиллиона"; genitivePlural = "октиллионов"; gender = Masculine }
        | _ -> defaultDimension

    let private getHundredNameInRussian hundred =
        match hundred with
        | 1 -> seq { "сто" }
        | 2 -> seq { "двести" }
        | 3 -> seq { "триста" }
        | 4 -> seq { "четыреста" }
        | 5 -> seq { "пятьсот" }
        | 6 -> seq { "шестьсот" }
        | 7 -> seq { "семьсот" }
        | 8 -> seq { "восемьсот" }
        | 9 -> seq { "девятьсот" }
        | _ -> Seq.empty

    let private getTenNameInRussian ten =
        match ten with
        | 2 -> seq { "двадцать" }
        | 3 -> seq { "тридцать" }
        | 4 -> seq { "сорок" }
        | 5 -> seq { "пятьдесят" }
        | 6 -> seq { "шестьдесят" }
        | 7 -> seq { "семьдесят" }
        | 8 -> seq { "восемьдесят" }
        | 9 -> seq { "девяносто" }
        | _ -> Seq.empty

    let private lessThenTwentyToWordsInRussian lessThenTwenty dimensionGender =
        match (lessThenTwenty, dimensionGender) with
        | (1, Feminine) -> seq { "одна" }
        | (1, Neuter) -> seq { "одно" }
        | (1, _) -> seq { "один" }
        | (2, Feminine) -> seq { "две" }
        | (2, _) -> seq { "два" }
        | (3, _) -> seq { "три" }
        | (4, _) -> seq { "четыре" }
        | (5, _) -> seq { "пять" }
        | (6, _) -> seq { "шесть" }
        | (7, _) -> seq { "семь" }
        | (8, _) -> seq { "восемь" }
        | (9, _) -> seq { "девять" }
        | (10, _) -> seq { "десять" }
        | (11, _) -> seq { "одиннадцать" }
        | (12, _) -> seq { "двенадцать" }
        | (13, _) -> seq { "тринадцать" }
        | (14, _) -> seq { "четырнадцать" }
        | (15, _) -> seq { "пятнадцать" }
        | (16, _) -> seq { "шестнадцать" }
        | (17, _) -> seq { "семнадцать" }
        | (18, _) -> seq { "восемнадцать" }
        | (19, _) -> seq { "девятнадцать" }
        | _ -> Seq.empty

    let private getDimensionWord lessThenTwenty dimension =
        if lessThenTwenty = 1 then dimension.nominative
        elif lessThenTwenty >= 2 && lessThenTwenty <= 4 then dimension.genitiveSingular
        else dimension.genitivePlural

    let private twoDigitsGroupToWordsInRussian twoDigitsGroup dimension =
        if twoDigitsGroup >= 20 then (twoDigitsGroup / Ten, twoDigitsGroup % Ten)
        else (Zero, twoDigitsGroup)
        |> fun (ten, lessThenTwenty) -> seq {
            yield! getTenNameInRussian ten
            yield! lessThenTwentyToWordsInRussian lessThenTwenty dimension.gender

            let dimensionWord = getDimensionWord lessThenTwenty dimension
            if dimensionWord <> Empty then dimensionWord
        }

    let private threeDigitsGroupToWordsInRussian (threeDigitsGroup, dimension) =
        seq {
            yield! getHundredNameInRussian (threeDigitsGroup / Hundred)
            yield! twoDigitsGroupToWordsInRussian (threeDigitsGroup % Hundred) dimension
        }

    let private splitIntoThreeDigitsGroups (source, defaultDimension) =
        seq {
            let mutable position = 0
            let mutable value = source
            while value > ZeroUL do
                let group = value % ThousandUL |> int32
                if position = 0 || group <> Zero then
                    let dimension = getDimensionRussianWord position defaultDimension
                    (group, dimension)

                value <- value / ThousandUL
                position <- position + 1
        }

    let private getNonZeroRussianWords value =
        value
        |> splitIntoThreeDigitsGroups
        |> Seq.rev
        |> Seq.map threeDigitsGroupToWordsInRussian
        |> Seq.concat

    let private getZeroRussianWords dimension =
        seq {
            "ноль"
            let dimensionWord = getDimensionWord Zero dimension
            if dimensionWord <> Empty then dimensionWord
        }

    let internal internalToWordsInRussian (value, dimension) =
        if value = ZeroUL then getZeroRussianWords dimension
        else getNonZeroRussianWords (value, dimension)
        |> String.concat WhiteSpace
