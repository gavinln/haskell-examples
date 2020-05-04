# Python equivalent of the files in the functors-applicatives-monads directory
# Haskell programs 01-main.hs to 12-main.hs

from typing import Optional
from typing import Iterable
from typing import Iterator
from typing import Any
from functools import partial


def input_str(message):
    year = input(message)
    if len(year) == 0:
        return None
    return year


def input_int(message):
    try:
        year = int(input(message))
    except ValueError:
        return None
    return year


def main1():
    ' no error handling '
    year = input_str("Please enter your birth year ")
    print("In 2020, you will be: ", (2020 - int(year)))


def main2():
    ' error handling '
    year = input_int("Please enter your birth year ")
    if year:
        print("In 2020, you will be: ", (2020 - year))
    else:
        print("You provided an invalid year")


def displayAge(age: Optional[int]) -> None:
    if age is None:
        print("You provided an invalid year")
    else:
        print("In that year, you will be: ", age)


def main3() -> None:
    ' separate function to display age '
    year: Optional[int] = input_int("Please enter your birth year ")
    maybeAge: Optional[int] = 2020 - year if year else None
    displayAge(maybeAge)


def yearToAge(year: Optional[int]) -> Iterator[Optional[int]]:
    if year is None:
        yield None
    validYear: Any = year
    yield 2020 - validYear


def main4() -> None:
    ' function to convert birth year to age '
    year: Optional[int] = input_int("Please enter your birth year ")
    maybeAge: Optional[int] = next(yearToAge(year))
    displayAge(maybeAge)


def main5() -> None:
    ' supports Monads but not need in imperative languages '
    pass


def main6() -> None:
    ' sequence operations: birth year and future year '
    # Does not handle invalid input
    birthYear: Any = input_int("Please enter your birth year ")
    futureYear: Any = input_int("Please enter some year in the future ")
    maybeAge: Optional[int] = futureYear - birthYear
    displayAge(maybeAge)


def yearDiff(futureYear: Optional[int],
             birthYear: Optional[int]) -> Iterator[Optional[int]]:
    if birthYear is None or futureYear is None:
        yield None
    futureYearValid: Any = futureYear
    birthYearValid: Any = birthYear
    validDiff: int = futureYearValid - birthYearValid
    yield validDiff


def main7() -> None:
    ' sequence operations and separate function: birth year and future year '
    # Correctly handles invalid input
    birthYear: Optional[int] = input_int("Please enter your birth year ")
    futureYear: Optional[int] = input_int("Please enter some year in the future ")
    maybeAge: Optional[int] = next(yearDiff(futureYear, birthYear))
    displayAge(maybeAge)


def main8() -> None:
    ' partial application '
    # Correctly handles invalid input
    birthYear: Optional[int] = input_int("Please enter your birth year ")
    futureYear: Optional[int] = input_int("Please enter some year in the future ")
    yearToAge = partial(yearDiff, futureYear)
    maybeAge: Optional[int] = next(yearToAge(birthYear))
    displayAge(maybeAge)


def main():
    main8()


if __name__ == '__main__':
    main()
