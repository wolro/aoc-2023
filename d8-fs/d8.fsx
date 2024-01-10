// Solution for Advent of Code, day 8
// https://adventofcode.com/2023/day/8
//
// Date: 2024-01-11
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


// ------------------------------------- Input parsing


// ------------------------------------- Solution, part 1


// ------------------------------------- Solution, part 2


// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Score (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Score - multiply number of winning strategies (Input): %A\n" (readInput input |> p1Result)

// printfn "Part 2 ---------------------------------------------------------- "
// printfn "Score with Jokers (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Score with Jokers (Input): %A" (readInput input |> p2Result)
