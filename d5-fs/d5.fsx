// Solution for Advent of Code, day 5
// https://adventofcode.com/2023/day/5
//
// Date: 2023-12-14
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
// printfn "Card pile worth in points (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Card pile worth in points (Input): %A" (readInput input |> p1Result)

// printfn "\nPart 2---------------------------------------------------------- "
// printfn "Number of cards won (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Number of cards won (Input): %A" (readInput input |> p2Result)
