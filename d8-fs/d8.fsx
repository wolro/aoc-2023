// Solution for Advent of Code, day 8
// https://adventofcode.com/2023/day/8
//
// Date: 2024-01-11
// Author: Wolfgang Rohringer

open System
open System.IO


// ------------------------------------- Types
type Node = { cur: string; l: string; r: string }


// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }


// ------------------------------------- Helper functions


// ------------------------------------- Input parsing
let parseNode (input: string) : Node =
    let splitEq = input.Split('=')
    let lr = splitEq[1].Split(',')

    { Node.cur = splitEq[0].Trim()
      l = lr[0] |> Seq.filter Char.IsLetterOrDigit |> String.Concat
      r = lr[1] |> Seq.filter Char.IsLetterOrDigit |> String.Concat }

let parseInput (inputs: string seq) : string * Node seq =
    let path = Seq.item 0 inputs
    let graph = Seq.skip 2 inputs |> Seq.map parseNode

    (path, graph)

// ------------------------------------- Solution, part 1
let traverseGraph (graphInput: string * Node seq) : int =
    let (path, graph) = graphInput
    // MISSING: repeat pattern if we don't hit "ZZZ" after first path finish
    let rec nextNode (path: string) (node: Node) (step: int) =
        if node.cur <> "ZZZ" then
            match path[step] with
            | 'L' -> nextNode path (graph |> Seq.filter (fun ele -> ele.cur = node.l) |> Seq.item 0) (step + 1)
            | 'R' -> nextNode path (graph |> Seq.filter (fun ele -> ele.cur = node.r) |> Seq.item 0) (step + 1)
            | _ -> failwith ("Error during graph traversal.")
        else
            step

    nextNode path (Seq.item 0 graph) 0


// ------------------------------------- Solution, part 2


// ------------------------------------- Main script

let inputTest1 = @".\input_test2.txt"
let input = @".\input.txt"

printfn "%A" (readInput inputTest1 |> parseInput |> traverseGraph)


// let (path, graph) = readInput inputTest1 |> parseInput

// printfn "%A" path
// graph |> Seq.iter (printfn "%A")



// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Score (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Score - multiply number of winning strategies (Input): %A\n" (readInput input |> p1Result)

// printfn "Part 2 ---------------------------------------------------------- "
// printfn "Score with Jokers (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Score with Jokers (Input): %A" (readInput input |> p2Result)
