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
    seq
    |> Seq.skip startIdx
    |> Seq.take (endIdx - startIdx + 1)


// ------------------------------------- Input parsing

// ------------------------------------- Solution, part 1
// 1) check if seed is in any source range (hopefully only one or 0)
// 2) map (just an offset)
// 3) check down the chain and repeat
// 4) repeat for all seeds

// ------------------------------------- Solution, part 2

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

let inputLines = readInput inputTest1

let mapTypes = [|"seed-to-soil";
     "soil-to-fertilizer";
     "fertilizer-to-water";
     "water-to-light";
     "light-to-temperature"; 
     "temperature-to-humidity"; 
     "humidity-to-location"|]

let seeds =
     (inputLines |> Seq.item 0).Split(' ') 
     |> Seq.map (filterNumbers >> System.UInt32.TryParse)
     |> Seq.filter (fun ele -> fst ele)
     |> Seq.map (fun ele -> snd ele)
     |> List.ofSeq

let getBorderIdx (inputLines: seq<string>) (mapType: string) = 
    inputLines 
    |> Seq.findIndex (fun line -> line.Contains(mapType))

let mapBorders =
    Seq.ofArray mapTypes
    |> Seq.map (fun mapType -> getBorderIdx inputLines mapType)
    |> Array.ofSeq


let mapDict:Dictionary<string, list<list<uint32>>> = 
    let seedSoilmaps = 
        sliceSeq (mapBorders[0]+1) (mapBorders[1]-2) inputLines
        |> Seq.map (fun ele -> ele.Split(' ')
                                |> Seq.map (System.UInt32.TryParse)
                                |> Seq.filter (fun ele -> fst ele)
                                |> Seq.map (fun ele -> snd ele)
                                |> List.ofSeq)
        |> List.ofSeq



    seq {(mapTypes[0], seedSoilmaps)} |> dict |> Dictionary

    

printfn "%A" (mapDict["seed-to-soil"])


// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Card pile worth in points (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Card pile worth in points (Input): %A" (readInput input |> p1Result)

// printfn "\nPart 2---------------------------------------------------------- "
// printfn "Number of cards won (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Number of cards won (Input): %A" (readInput input |> p2Result)
