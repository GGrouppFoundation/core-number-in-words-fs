namespace GGroupp.Core.Tests

open Xunit

module internal TestHelper =

    let internal shouldBeEqual expected actual =
        Assert.Equal(expected = expected, actual = actual)