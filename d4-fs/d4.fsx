// Solution for Advent of Code, day 4
// https://adventofcode.com/2023/day/4
//
// Date: 2023-12-12
// Author: Wolfgang Rohringer

open System.IO
open System.Collections.Generic

// ------------------------------------- Types

type Card =
    { id: int
      winning: int list
      draw: int list }

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
let parseCard (str: string) =

    let extractNumbers (inStr: string) =
        inStr.Split(' ')
        |> Seq.map (filterNumbers >> System.Int32.TryParse)
        |> Seq.filter (fun ele -> fst ele)
        |> Seq.map (fun ele -> snd ele)
        |> List.ofSeq


    match str.Split(':', '|') with
    | [| idStr; winStr; drawStr |] ->
        { Card.id = (idStr |> filterNumbers |> System.Int32.Parse)
          winning = extractNumbers winStr
          draw = extractNumbers drawStr }
    | _ -> failwith "Invalid game input string."


// ------------------------------------- Solution, part 1
let nrWinNum (card: Card) =
    (Set.intersect (Set.ofList card.winning) (Set.ofList card.draw)).Count

let calcPoints (nrCount: int) =
    match nrCount with
    | 0 -> int 0.0
    | _ -> int (2.0 ** ((float nrCount) - 1.0))

let p1Result (cards: seq<string>) =
    cards |> Seq.map (parseCard >> nrWinNum >> calcPoints) |> Seq.sum


// ------------------------------------- Solution, part 2

let updateCardNumbers (card: Card) (cardNumbers: IDictionary<int, int>) =
    let wins = card |> nrWinNum

    seq { 1..wins }
    |> Seq.iter (fun ele ->
        let startId = ele + card.id

        if startId <= cardNumbers.Count then
            (cardNumbers[startId] <- cardNumbers[startId] + 1))

let p2Result (cardStrs: seq<string>) =
    let cards = cardStrs |> Seq.map (parseCard)

    let cardNumbers: Dictionary<int, int> =
        cards |> Seq.map (fun ele -> (ele.id, 1)) |> dict |> Dictionary

    cards
    |> Seq.iter (fun card ->
        seq { 1 .. cardNumbers[card.id] }
        |> Seq.iter (fun _ -> updateCardNumbers card cardNumbers))

    cardNumbers |> Seq.map (fun ele -> ele.Value) |> Seq.sum

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

printfn "Part 1 ---------------------------------------------------------- "
printfn "Card pile worth in points (test input): %A" (readInput inputTest1 |> p1Result)
printfn "Card pile worth in points (Input): %A" (readInput input |> p1Result)

printfn "\nPart 2---------------------------------------------------------- "
printfn "Number of cards won (test input): %A" (readInput inputTest1 |> p2Result)
printfn "Number of cards won (Input): %A" (readInput input |> p2Result)
