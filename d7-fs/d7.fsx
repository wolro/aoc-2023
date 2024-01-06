// Solution for Advent of Code, day 7
// https://adventofcode.com/2023/day/7
//
// Date: 2024-01-06
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types


// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

// ------------------------------------- Helper functions
let filterNumbers (entry: string) = String.filter System.Char.IsDigit entry


// ------------------------------------- Input parsing


// ------------------------------------- Solution, part 1


// ------------------------------------- Solution, part 2


// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"


// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Score - multiply number of winning strategies (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Score - multiply number of winning strategies (Input): %A\n" (readInput input |> p1Result)

// printfn "Part 2 ---------------------------------------------------------- "
// printfn "Score - multiply number of winning strategies (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Score - multiply number of winning strategies (Input): %A" (readInput input |> p2Result)
