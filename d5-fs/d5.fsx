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
        |> Dictionary

    let lastType = mapTypes[mapTypes.Length - 1]
    let lastBorder = getBorderIdx inputLines lastType
    let lastEntries = sliceSeq (lastBorder + 1) (Seq.length inputLines - 1) inputLines
    mapDict[lastType] <- lastEntries |> Seq.map filterAndParse |> List.ofSeq
    let seeds = inputLines |> Seq.item 0 |> filterAndParse |> List.ofSeq

    seeds, mapDict

// ------------------------------------- Solution, part 1

/// <summary> This is the procedural version for looking up the location that I came up with initially.
/// Works fine. Actually great that F# supports this stuff as well. But this is not what I signed up for.</summary>
let lookupLocation_proc (mapDict: IDictionary<string, list<list<uint32>>>) (seed: uint32) : uint32 =
    let mutable number = seed
    let mapIdcs = [| 0 .. ((mapDict.Count) - 1) |]

    for mapIdx in mapIdcs do

        let curMap = mapDict[mapTypes[mapIdx]]
        let checkIdcs = [| 0 .. ((curMap.Length - 1)) |]
        let mutable entryModified = false

        for checkIdx in checkIdcs do
            if
                (number >= curMap[checkIdx][1])
                && (number <= curMap[checkIdx][1] + curMap[checkIdx][2])
                && (not entryModified)
            then
                number <- (number + curMap[checkIdx][0] - curMap[checkIdx][1])
                entryModified <- true

    number


/// <summary> This is the functional version for the lookup after some playing around.
/// This version is used for the solution of part 1.</summary>
let lookupLocation (mapDict: IDictionary<string, list<list<uint32>>>) (seed: uint32) : uint32 =
    let updateEntry (number: uint32) (curMap: list<list<uint32>>) =
        curMap
        |> List.tryFind (fun mapEntry -> number >= mapEntry[1] && number <= mapEntry[1] + mapEntry[2])
        |> Option.map (fun check -> number + check[0] - check[1])
        |> Option.defaultValue number

    mapDict.Values |> Seq.fold (fun entry curMap -> updateEntry entry curMap) seed

let p1Result (input: seq<string>) : uint32 =
    let (seeds, mapDict) = parseInput input
    seeds |> List.map (lookupLocation mapDict) |> List.reduce min

// ------------------------------------- Solution, part 2

/// <summary> This is the procedural version for the reverse lookup. Still had to start with this
/// before being able to adapt the more functional solution "reverseLookup"</summary>
let reverseLookup_proc (mapDict: IDictionary<string, list<list<uint32>>>) (location: uint32) : uint32 =
    let mutable number = location
    let mapIdcs = [| ((mapDict.Count) - 1) .. (-1) .. 0 |]

    for mapIdx in mapIdcs do

        let curMap = mapDict[mapTypes[mapIdx]]
        let checkIdcs = [| 0 .. ((curMap.Length - 1)) |]
        let mutable entryModified = false

        for checkIdx in checkIdcs do
            if
                (number >= curMap[checkIdx][0])
                && (number <= curMap[checkIdx][0] + curMap[checkIdx][2])
                && (not entryModified)
            then
                number <- (number + curMap[checkIdx][1] - curMap[checkIdx][0])
                entryModified <- true

    number

/// <summary> The more functional version for the reverse lookup. The only ugliness is
/// the gymnastics to cast the "ICollection" into a "list" via detour as a seq. </summary>
let reverseLookup (mapDict: IDictionary<string, list<list<uint32>>>) (location: uint32) : uint32 =
    let updateEntry (number: uint32) (curMap: list<list<uint32>>) =
        curMap
        |> List.tryFind (fun mapEntry -> number >= mapEntry[0] && number <= mapEntry[0] + mapEntry[2])
        |> Option.map (fun check -> number + check[1] - check[0])
        |> Option.defaultValue number

    mapDict.Values
    |> Seq.cast
    |> List.ofSeq
    |> List.rev
    |> Seq.fold (fun entry curMap -> updateEntry entry curMap) location

/// <summary> We do a reverse lookup starting from location 0 upwards until
/// we find a number that is within the seed ranges (i.e. between "leftBorder[i]"
/// and "rightBorder[i]" for any seed range denoted by index i. This should
/// be faster than the "normal" lookup starting from almost 3e9 seed values.
///
/// Here, I won't bother making it more functional at this point, since
/// I have had enough of this day's puzzle.</summary>
let p2Result (input: seq<string>) : uint32 =

    let (seeds, mapDict) = parseInput input

    let leftBorders =
        seeds
        |> List.mapi (fun i ele -> ele, i)
        |> List.filter (fun (ele, i) -> i % 2 = 0)
        |> List.map fst

    let rightBorders =
        seeds
        |> List.mapi (fun i ele -> ele, i)
        |> List.filter (fun (ele, i) -> i % 2 = 1)
        |> List.map fst

    let mutable location = 0u
    let mutable breakCondition = false
    let mutable number = 0u

    while not breakCondition do

        let result = reverseLookup mapDict location

        let validSeed =
            leftBorders
            |> List.mapi (fun i ele -> (result >= ele) && (result <= ele + rightBorders[i]))
            |> List.reduce (||)

        if validSeed then
            breakCondition <- true
            number <- location

        location <- location + 1u

    number

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

printfn "Part 1 ---------------------------------------------------------- "
printfn "Lowest location number (test input): %A" (readInput inputTest1 |> p1Result)
printfn "Lowest location number (Input): %A" (readInput input |> p1Result)

printfn "Part 2 ---------------------------------------------------------- "
printfn "Lowest location number (test input): %A" (readInput inputTest1 |> p2Result)
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
printfn "Lowest location number (Input): %A" (readInput input |> p2Result)
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
