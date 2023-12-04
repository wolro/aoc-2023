// Solution for Advent of Code, day 1
// https://adventofcode.com/2023/day/1
//
// Date: 2023-12-03
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types
type SearchOrder =
    | Up
    | Down

// ------------------------------------- Helper functions

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }


let filterNumbers (entry: string) = String.filter System.Char.IsDigit entry


let digitsFromStrings (entry: string) =
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


/// <summary> In part 2, we need to scan the strings from both sides. Simply
/// applying "digitsFromStrings" ruins some relevant spelled-out numbers,
/// depending on the order of the replacement. </summary>
let rec returnEdgeDigits (entry: string) (idx: int) (strideMax: int) (sOrd: SearchOrder) : (string) =

    let stride = strideMax
    let entryChunk = entry.[0 + idx .. stride + idx]
    let entryChunkNum = (digitsFromStrings entryChunk)

    let startIdx =
        if sOrd = Up then 0
        else if entryChunk.Length >= stride then stride - 1
        else entryChunk.Length - 1 // otherwise out of bounds if we hit the beginning of the string from the right

    match sOrd, System.Char.IsDigit entryChunk.[startIdx] with
    | _, true -> string entryChunk.[startIdx] // Already a digit on the edge, no work required
    | _, _ when entryChunk <> entryChunkNum || entryChunk.Length = 0 -> entryChunkNum // Found a spelled-out digit or empty chunk
    | Up, _ -> returnEdgeDigits entry (idx + 1) strideMax sOrd // continue upwards
    | Down, _ -> returnEdgeDigits entry (idx - 1) strideMax sOrd // continue downwards


/// <summary> Return digits relevant for part 2 calibration values. </summary>
let allDigitsFromStrings (stride: int) (entry: string) : string =
    (returnEdgeDigits entry 0 stride SearchOrder.Up)
    + (returnEdgeDigits entry (entry.Length - stride) stride SearchOrder.Down)

let getCalVal (entry: string) =
    string entry.[0]
    + (if (entry.Length > 1) then
           string entry.[entry.Length - 1]
       else
           string entry.[0])

// ------------------------------------- Solution

/// <summary>Solution for both parts, only differs by "convFcn".</summary>
let solution (inputFile: string) (convFcn: string -> string) : int64 =

    let calEntries = readInput inputFile

    calEntries
    |> Seq.map (convFcn)
    |> Seq.map (filterNumbers)
    |> Seq.map (getCalVal)
    |> Seq.map (System.Int64.Parse)
    |> Seq.sum

// ------------------------------------- Main script
let inputTest1 = @".\input_test1.txt"
let inputTest2 = @".\input_test2.txt"
let input = @".\input.txt"
let stride_max = 5

printfn "P1 solution for test input: %A" (solution inputTest1 id) // "id" = unity. No conversion function.
printfn "P1 solution: %A" (solution input id)
printfn "P2 solution for test input: %A" (solution inputTest2 (allDigitsFromStrings stride_max)) // Note: curry
printfn "P2 solution: %A" (solution input (allDigitsFromStrings stride_max))
