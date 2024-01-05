// Solution for Advent of Code, day 5
// https://adventofcode.com/2023/day/5
//
// Date: 2023-12-14
// Author: Wolfgang Rohringer

open System.IO
open System.Collections.Generic

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

let sliceSeq startIdx endIdx (seq: 'a seq) =
    seq |> Seq.skip startIdx |> Seq.take (endIdx - startIdx + 1)


// ------------------------------------- Input parsing


// ------------------------------------- Solution, part 1



// ------------------------------------- Solution, part 2


// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"



// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Lowest location number (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Lowest location number (Input): %A" (readInput input |> p1Result)

// printfn "Part 2 ---------------------------------------------------------- "
// printfn "Lowest location number (test input): %A" (readInput inputTest1 |> p2Result)
// let stopWatch = System.Diagnostics.Stopwatch.StartNew()
// printfn "Lowest location number (Input): %A" (readInput input |> p2Result)
// stopWatch.Stop()
// printfn "%f" stopWatch.Elapsed.TotalMilliseconds
