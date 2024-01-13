// Solution for Advent of Code, day 8
// https://adventofcode.com/2023/day/8
//
// Date: 2024-01-11
// Author: Wolfgang Rohringer

#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"

open System
open System.IO
open MathNet.Numerics

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

    let rec nextNode (path: string) (node: Node) (step: int) =
        if node.cur <> "ZZZ" then
            match path[step % path.Length] with
            | 'L' -> nextNode path (graph |> Seq.filter (fun ele -> ele.cur = node.l) |> Seq.item 0) (step + 1)
            | 'R' -> nextNode path (graph |> Seq.filter (fun ele -> ele.cur = node.r) |> Seq.item 0) (step + 1)
            | _ -> failwith ("Error during graph traversal.")
        else
            step

    let startNode = graph |> Seq.find (fun ele -> ele.cur = "AAA")
    nextNode path (startNode) 0

let p1Result (input: string seq) : int = input |> parseInput |> traverseGraph

// ------------------------------------- Solution, part 2

/// <summary> Let's try to do the traversal for different starting points in parallel using async syntax. </summary>
let traverseGraphP2 (graphInput: string * Node seq) : int array =
    let (path, graph) = graphInput

    let rec nextNode (path: string) (node: Node) (step: int) =
        if node.cur[2] <> 'Z' then
            match path[step % path.Length] with
            | 'L' -> nextNode path (graph |> Seq.filter (fun ele -> ele.cur = node.l) |> Seq.item 0) (step + 1)
            | 'R' -> nextNode path (graph |> Seq.filter (fun ele -> ele.cur = node.r) |> Seq.item 0) (step + 1)
            | _ -> failwith ("Error during graph traversal.")
        else
            step

    let asyncTraversal startNode =
        async { return nextNode path (startNode) 0 }

    let startNodes = graph |> Seq.filter (fun ele -> ele.cur[2] = 'A')

    startNodes |> Seq.map asyncTraversal |> Async.Parallel |> Async.RunSynchronously

/// <summary> I have been spoilt by having read "lcm" in the context of day 8 while looking
/// for alternative solutions to previous days. So for sure I tried this and it worked, even
/// though from the puzzle description it is far from clear that this should work. It seems
/// that the input is specifically built for this to function. </summary>
let p2Result (input: string seq) : int64 =
    input
    |> parseInput
    |> traverseGraphP2 // steps for each path
    |> Seq.map int64 // Euclid.LeastCommonMultiple needs int64
    |> Seq.toArray // Euclid.LeastCommonMultiple needs an array
    |> Euclid.LeastCommonMultiple

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let inputTest2 = @".\input_test2.txt"
let inputTest3 = @".\input_test3.txt"
let input = @".\input.txt"


printfn "Part 1 ---------------------------------------------------------- "
printfn "Number of steps from AAA to ZZZ (test input 1): %A" (readInput inputTest1 |> p1Result)
printfn "Number of steps from AAA to ZZZ (test input 2): %A" (readInput inputTest2 |> p1Result)
printfn "Number of steps from AAA to ZZZ (input): %A" (readInput input |> p1Result)


printfn "Part 2 ---------------------------------------------------------- "
printfn "Number of steps for all paths to end up in a 'Z' field (test input): %A" (readInput inputTest3 |> p2Result)
printfn "Number of steps for all paths to end up in a 'Z' field (input): %A" (readInput input |> p2Result)
