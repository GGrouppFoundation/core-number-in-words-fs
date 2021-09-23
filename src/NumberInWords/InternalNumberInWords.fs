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

    let private getHundredWord hundred =
        match hundred with
        | 1 -> "сто"
        | 2 -> "двести"
        | 3 -> "триста"
        | 4 -> "четыреста"
        | 5 -> "пятьсот"
        | 6 -> "шестьсот"
        | 7 -> "семьсот"
        | 8 -> "восемьсот"
        | 9 -> "девятьсот"
        | _ -> Empty

    let private getTenWord ten =
        match ten with
        | 2 -> "двадцать"
        | 3 -> "тридцать"
        | 4 -> "сорок"
        | 5 -> "пятьдесят"
        | 6 -> "шестьдесят"
        | 7 -> "семьдесят"
        | 8 -> "восемьдесят"
        | 9 -> "девяносто"
        | _ -> Empty

    let private getLessThenTwentyWord lessThenTwenty dimensionGender =
        match (lessThenTwenty, dimensionGender) with
        | (1, Feminine) -> "одна"
        | (1, Neuter) -> "одно"
        | (1, _) -> "один"
        | (2, Feminine) -> "две"
        | (2, _) -> "два"
        | (3, _) -> "три"
        | (4, _) -> "четыре"
        | (5, _) -> "пять"
        | (6, _) -> "шесть"
        | (7, _) -> "семь"
        | (8, _) -> "восемь"
        | (9, _) -> "девять"
        | (10, _) -> "десять"
        | (11, _) -> "одиннадцать"
        | (12, _) -> "двенадцать"
        | (13, _) -> "тринадцать"
        | (14, _) -> "четырнадцать"
        | (15, _) -> "пятнадцать"
        | (16, _) -> "шестнадцать"
        | (17, _) -> "семнадцать"
        | (18, _) -> "восемнадцать"
        | (19, _) -> "девятнадцать"
        | _ -> Empty

    let private getNumberRankDimension numberRank zeroRankDimension =
        match numberRank with
        | 1 -> { Nominative = "тысяча"; GenitiveSingular = "тысячи"; GenitivePlural = "тысяч"; Gender = Feminine }
        | 2 -> { Nominative = "миллион"; GenitiveSingular = "миллиона"; GenitivePlural = "миллионов"; Gender = Masculine }
        | 3 -> { Nominative = "миллиард"; GenitiveSingular = "миллиарда"; GenitivePlural = "миллиардов"; Gender = Masculine }
        | 4 -> { Nominative = "триллион"; GenitiveSingular = "триллиона"; GenitivePlural = "триллионов"; Gender = Masculine }
        | 5 -> { Nominative = "квадриллион"; GenitiveSingular = "квадриллиона"; GenitivePlural = "квадриллионов"; Gender = Masculine }
        | 6 -> { Nominative = "квинтиллион"; GenitiveSingular = "квинтиллиона"; GenitivePlural = "квинтиллионов"; Gender = Masculine }
        | 7 -> { Nominative = "секстиллион"; GenitiveSingular = "секстиллиона"; GenitivePlural = "секстиллионов"; Gender = Masculine }
        | 8 -> { Nominative = "септиллион"; GenitiveSingular = "септиллиона"; GenitivePlural = "септиллионов"; Gender = Masculine }
        | 9 -> { Nominative = "октиллион"; GenitiveSingular = "октиллиона"; GenitivePlural = "октиллионов"; Gender = Masculine }
        | _ -> zeroRankDimension

    let private getDimensionWord lessThenTwenty dimension =
        match lessThenTwenty with
        | 1 -> dimension.Nominative
        | 2 | 3 | 4 -> dimension.GenitiveSingular
        | _ -> dimension.GenitivePlural

    let private twoDigitGroupToWordsInRussian twoDigitGroup dimension =
        if twoDigitGroup >= 20 then
            (twoDigitGroup / Ten, twoDigitGroup % Ten)
        else
            (Zero, twoDigitGroup)
        |> fun (ten, lessThenTwenty) -> seq {
            getTenWord ten
            getLessThenTwentyWord lessThenTwenty dimension.Gender
            getDimensionWord lessThenTwenty dimension
        }

    let private getThreeDigitGroupWords (threeDigitGroup, dimension) =
        seq {
            getHundredWord (threeDigitGroup / Hundred)
            yield! twoDigitGroupToWordsInRussian (threeDigitGroup % Hundred) dimension
        }

    let private splitIntoThreeDigitGroups (source, zeroRankDimension) =
        seq {
            let mutable rank = 0
            let mutable value = source
            while value > ZeroUL do
                let group = value % ThousandUL |> int32
                if rank = 0 || group <> Zero then
                    let dimension = getNumberRankDimension rank zeroRankDimension
                    (group, dimension)

                value <- value / ThousandUL
                rank <- rank + 1
        }

    let private isNotEmpty text = text <> Empty

    let internal internalGetRussianWords (value, dimension) =
        if value = ZeroUL then
            seq {
                "ноль"
                getDimensionWord Zero dimension
            }
        else
            (value, dimension)
            |> splitIntoThreeDigitGroups
            |> Seq.rev
            |> Seq.map getThreeDigitGroupWords
            |> Seq.concat
        |> Seq.filter isNotEmpty
        |> String.concat WhiteSpace
