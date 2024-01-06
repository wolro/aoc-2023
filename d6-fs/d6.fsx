// Solution for Advent of Code, day 6
// https://adventofcode.com/2023/day/6
//
// Date: 2024-01-06
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types

type DistRecord = { time: uint64; dist: uint64 }

// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

// ------------------------------------- Helper functions
let filterNumbers (entry: string) = String.filter System.Char.IsDigit entry

/// <summary> Get times and distances for each "race" according to part 1. </summary>
let extractNumbers (inStr: string) =
    inStr.Split(' ')
    |> Seq.map (filterNumbers >> System.UInt64.TryParse)
    |> Seq.filter (fun ele -> fst ele)
    |> Seq.map (fun ele -> snd ele)
    |> List.ofSeq

/// <summary> Get time and distance for the race according to part 2. </summary>
let extractNumber (inStr: string) =
    (inStr.Split(':')[1]).Replace(" ", "") |> System.UInt64.TryParse |> snd

// ------------------------------------- Input parsing

/// <summary> Decided to account for the difference between part 1 and 2 directly
/// during parsing. This is the parsing function for part 1. </summary>
let parseRecordsP1 (input: seq<string>) : list<DistRecord> =
    let times = input |> Seq.item 0 |> extractNumbers
    let distances = input |> Seq.item 1 |> extractNumbers

    let recs =
        times
        |> List.mapi (fun i ele ->
            { DistRecord.time = ele
              dist = distances[i] })

    recs

/// <summary> Decided to account for the difference between part 1 and 2 directly
/// during parsing. This is the parsing function for part 2. </summary>
let parseRecordsP2 (input: seq<string>) : DistRecord =
    { DistRecord.time = Seq.item 0 input |> extractNumber
      DistRecord.dist = Seq.item 1 input |> extractNumber }

// ------------------------------------- Solution, part 1

/// <summary> Create a range of possible "button hold times", calculate the
/// travel distance, compare with previous record and return the number
/// of "winning" button hold times.</summary)
let winningHoldTimes (distRec: DistRecord) : uint64 =

    let nrWinners =
        [ 1UL .. distRec.time ]
        |> List.map (fun ele -> (distRec.time - ele) * ele)
        |> List.filter (fun ele -> ele > distRec.dist)
        |> List.length
        |> uint64

    nrWinners

let p1Result (input: string seq) : uint64 =
    input |> parseRecordsP1 |> List.map (winningHoldTimes) |> List.reduce (*)


// ------------------------------------- Solution, part 2

let p2Result (input: string seq) : uint64 =
    input |> parseRecordsP2 |> winningHoldTimes

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"


printfn "Part 1 ---------------------------------------------------------- "
printfn "Score - multiply number of winning strategies (test input): %A" (readInput inputTest1 |> p1Result)
printfn "Score - multiply number of winning strategies (Input): %A\n" (readInput input |> p1Result)

printfn "Part 2 ---------------------------------------------------------- "
printfn "Score - multiply number of winning strategies (test input): %A" (readInput inputTest1 |> p2Result)
printfn "Score - multiply number of winning strategies (Input): %A" (readInput input |> p2Result)
