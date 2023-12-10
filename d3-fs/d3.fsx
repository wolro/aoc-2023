// Solution for Advent of Code, day 3
// https://adventofcode.com/2023/day/3
//
// Date: 2023-12-05
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types
type partNum = { value: string; x: int; y: int }

// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

// ------------------------------------- Helper functions
let filterNumbers (entry: string) = String.filter System.Char.IsDigit entry

let retrieveNums (inputLine: int * string) =
    let lineIdx, inputStr = inputLine

    let nums =
        inputStr.Split('.')
        |> Seq.map (filterNumbers)
        |> Seq.filter (fun ele -> not (System.String.IsNullOrEmpty ele))

    let idcs = nums |> Seq.map (fun num -> inputStr.IndexOf(num))
    let lineIdcs = Seq.init (Seq.length idcs) (fun _ -> lineIdx)

    Seq.zip3 lineIdcs idcs nums


let buildPartNum (numEntry: int * int * string) =
    let lineNr, idx, numVal = numEntry

    { partNum.value = numVal
      x = idx
      y = lineNr }


// ------------------------------------- Parsing input into types

// ------------------------------------- Solution, part 1

let symMask (schematicRaw: seq<string>) =
    schematicRaw
    |> Seq.map (
        Seq.map (fun ele ->
            if not (System.Char.IsDigit ele || ele = '.') then
                true
            else
                false)
    )
    |> array2D

let symThere (symbols: bool[,]) (partCandidate: partNum) =

    let lenX, lenY = (Array2D.length1 symbols, Array2D.length2 symbols)

    partCandidate.value
    |> Seq.mapi (fun idx _val ->
        let x = partCandidate.x
        let y = partCandidate.y

        (if y > 0 && x > 0 then
             symbols.[y - 1, x - 1 + idx]
         else
             false
         || if y > 0 && (x + idx) <= lenX then
                symbols.[y - 1, x + idx]
            else
                false
         || if y > 0 && (x + 1 + idx) <= lenX then
                symbols.[y - 1, x + 1 + idx]
            else
                false
         || if (y + 1) <= lenY && x > 0 then
                symbols.[y + 1, x - 1 + idx]
            else
                false
         || if (y + 1) <= lenY && (x + idx) <= lenX then
                symbols.[y + 1, x + idx]
            else
                false
         || if (y + 1) <= lenY && (x + 1 + idx) <= lenX then
                symbols.[y + 1, x + 1 + idx]
            else
                false
         || if x > 0 then symbols.[y, x - 1 + idx] else false
         || if (x + 1 + idx) <= lenX then
                symbols.[y, x + 1 + idx]
            else
                false))
    |> Seq.fold (||) false

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

let partNums =
    schematicRaw
    |> Seq.mapi (fun idcs ele -> retrieveNums (idcs, ele) |> Seq.map (buildPartNum))
    |> Seq.collect id


let symbols = symMask schematicRaw

let validMask = partNums |> Seq.map (symThere symbols)

let validNums =
    Seq.zip partNums validMask
    |> Seq.filter (fun ele -> (snd ele))
    |> Seq.map (fun ele -> (fst ele).value)
    |> Seq.map (System.Int32.Parse)

// let result = Seq.sum validNums

validNums |> Seq.iter (printfn "%A")
// printfn "%A" result

// printfn "Part 1 ---------------------------------------------------------- "

// printfn "\nPart 2---------------------------------------------------------- "
