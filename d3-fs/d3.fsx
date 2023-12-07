// Solution for Advent of Code, day 3
// https://adventofcode.com/2023/day/3
//
// Date: 2023-12-05
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types
type partNum = { value: int; x: int; y: int }

// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

// ------------------------------------- Helper functions
let filterNumbers (entry: string) = String.filter System.Char.IsDigit entry


// ------------------------------------- Parsing input into types

// ------------------------------------- Solution, part 1
// Basic idea:
// Parse into 2d char array (symbols get letters)
// Build mask of symbols and numbers
// Write symbol coordinates into an array
// For this array, check number mask for neighbor coordinates indicates a number.
//      If yes, "get this number" (need to figure out that part) , and add them to the sum.


// ------------------------------------- Solution, part 2

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

let schematicRaw = readInput inputTest1

let numsOnly =
    schematicRaw
    |> Seq.map (Seq.map (fun ele -> if System.Char.IsDigit ele then ele else '.'))
    |> Seq.map (Seq.toArray)
    |> Seq.map (System.String)
    |> Seq.map (fun ele -> ele.Split('.'))

let symMask =
    schematicRaw
    |> Seq.map (
        Seq.map (fun ele ->
            if not (System.Char.IsDigit ele || ele = '.') then
                true
            else
                false)
    )
    |> array2D

// let partNums =
//     schematicRaw
//     |> Seq.map (fun ele -> [ for ch in ele -> ch ])
//     |> Seq.iter (List.find (fun ch -> System.Char.IsDigit ch))

printfn "%A" numsOnly
// printfn "%A" numMask[0, 3]
// printfn "%A" numMask[0, 5]

printfn "Part 1 ---------------------------------------------------------- "

printfn "\nPart 2---------------------------------------------------------- "
