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

let mapTypes =
    [| "seed-to-soil"
       "soil-to-fertilizer"
       "fertilizer-to-water"
       "water-to-light"
       "light-to-temperature"
       "temperature-to-humidity"
       "humidity-to-location" |]

let getBorderIdx (inputLines: seq<string>) (mapType: string) =
    inputLines |> Seq.findIndex (fun line -> line.Contains(mapType))


let parseInput (inputLines: seq<string>) : (list<uint32> * IDictionary<string, list<list<uint32>>>) =
    let filterAndParse (line: string) =
        line.Split(' ')
        |> Seq.map (filterNumbers >> System.UInt32.TryParse)
        |> Seq.filter fst
        |> Seq.map snd
        |> List.ofSeq

    let mapDict =
        mapTypes
        |> Array.windowed 2
        |> Array.map (fun ele ->
            match ele with
            | [| currentType; nextType |] ->
                currentType,
                sliceSeq (getBorderIdx inputLines currentType + 1) (getBorderIdx inputLines nextType - 2) inputLines
                |> Seq.map filterAndParse
                |> List.ofSeq
            | _ -> failwith "Invalid array length in pattern match")

        |> dict

    let seeds = inputLines |> Seq.item 0 |> filterAndParse |> List.ofSeq

    seeds, mapDict
// ------------------------------------- Solution, part 1
// 1) check if seed is in any source range (hopefully only one or 0)
// 2) map (just an offset)
// 3) check down the chain and repeat
// 4) repeat for all seeds

// ------------------------------------- Solution, part 2

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

let (seeds, mapDict) = inputTest1 |> readInput |> parseInput

// printfn "%A" (mapDict["temperature-to-humidity"])
let seedIdx = 2
let checkIdx = 1
let mapIdx = 0

if
    (seeds[seedIdx] >= (mapDict[mapTypes[mapIdx]]).[checkIdx][1])
    && (seeds[seedIdx]
        <= (mapDict[mapTypes[mapIdx]]).[checkIdx][1]
           + (mapDict[mapTypes[mapIdx]]).[checkIdx][2])
then
    printfn
        "%A"
        (seeds[seedIdx] + (mapDict[mapTypes[mapIdx]]).[checkIdx][0]
         - (mapDict[mapTypes[mapIdx]]).[checkIdx][1])



// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Card pile worth in points (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Card pile worth in points (Input): %A" (readInput input |> p1Result)

// printfn "\nPart 2---------------------------------------------------------- "
// printfn "Number of cards won (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Number of cards won (Input): %A" (readInput input |> p2Result)
