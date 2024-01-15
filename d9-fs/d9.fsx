// Solution for Advent of Code, day 9
// https://adventofcode.com/2023/day/9
//
// Date: 2024-01-13
// Author: Wolfgang Rohringer

open System
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
// let input = @".\input.txt"

let history (histStr: string) =
    histStr.Split(' ') |> Seq.map System.Int32.Parse


let histories = readInput inputTest1 |> Seq.map (history)

let rec allIdentical (seq: 'a seq) (comparer: 'a -> 'a -> bool) =
    match Seq.isEmpty seq with
    | true -> true
    | false -> Seq.tail seq |> Seq.forall (comparer (Seq.head seq))

let diff (seq: seq<_>) =
    seq |> Seq.pairwise |> Seq.map (fun (x, y) -> y - x)

let rec reduceHistory (step: int) (history: int seq) : (int * int seq) =
    let hDiff = history |> diff

    match (allIdentical hDiff (=)) with
    | false -> reduceHistory (step + 1) hDiff
    | true -> (step + 1, (hDiff))


let selHistRaw = Seq.item 0 histories
let selHist = selHistRaw |> Seq.map (fun ele -> ele - (Seq.item 0 selHistRaw))
// selHist |> Seq.iter (printfn "%A")
// printfn ("")

let (step, hDiff) = selHist |> (reduceHistory 0)

// hDiff |> Seq.iter (printfn "%A")

printfn ""
printfn "%A" step
printfn "%A" hDiff

let rec rebuildHistory (step: int) (hDiff: int seq) : int seq =
    match step with
    | 0 -> hDiff
    | _ -> rebuildHistory (step - 1) (Seq.scan (+) (Seq.item 0 hDiff) hDiff)


hDiff
|> Seq.append (seq { Seq.item 0 hDiff })
|> rebuildHistory step
|> Seq.iter (printfn "%A")
// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Number of steps from AAA to ZZZ (test input 1): %A" (readInput inputTest1 |> p1Result)
// printfn "Number of steps from AAA to ZZZ (input): %A" (readInput input |> p1Result)


// printfn "Part 2 ---------------------------------------------------------- "
// printfn "Number of steps for all paths to end up in a 'Z' field (test input): %A" (readInput inputTest3 |> p2Result)
// printfn "Number of steps for all paths to end up in a 'Z' field (input): %A" (readInput input |> p2Result)
