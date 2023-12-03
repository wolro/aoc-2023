// Solution for Advent of Code, day 1
// https://adventofcode.com/2023/day/1
//
// Date: 2023-12-03
// Author: Wolfgang Rohringer

open System.IO

// Helper functions

let readInput path =
    seq { use reader = new StreamReader(File.OpenRead(path))
          while not reader.EndOfStream do
            yield reader.ReadLine()
            }

let filterNumbers (entry: string) =
    String.filter System.Char.IsDigit entry

let numbersFromStrings (entry: string) =
    entry
        .Replace("nine", "9")
        .Replace("eight", "8")
        .Replace("seven", "7")
        .Replace("six", "6")
        .Replace("five", "5")
        .Replace("four", "4")
        .Replace("three", "3")
        .Replace("two", "2")
        .Replace("one", "1")

let getCalVal (entry: string) =
    string entry.[0] + (if (entry.Length > 1) then string entry.[entry.Length-1] else string entry.[0])


/// Solution for part 1
let solution (inputFile: string) (convertFcn: string -> string): int64 =

    let calEntries = readInput inputFile

    calEntries
        |> Seq.map(convertFcn)
        |> Seq.map(filterNumbers)
        |> Seq.map(getCalVal)
        |> Seq.map(System.Int64.Parse)
        |> Seq.sum

// Main script
let inputTest1 = @".\input_test1.txt"
let inputTest2 = @".\input_test2.txt"
let input = @".\input.txt"

printfn "P1 solution for test input: %A" (solution inputTest1 id)
printfn "P1 solution: %A" (solution input id)
printfn "P2 solution for test input: %A" (solution inputTest2 numbersFromStrings)
printfn "P2 solution: %A" (solution input numbersFromStrings)

let calEntries = readInput inputTest2

let calValues = calEntries
              |> Seq.map(numbersFromStrings)
              |> Seq.map(filterNumbers)
              |> Seq.map(getCalVal)

printfn "%A" (Seq.item 4 calValues)