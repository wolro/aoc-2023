// Solution for Advent of Code, day 7
// https://adventofcode.com/2023/day/7
//
// Date: 2024-01-06
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types

type HandValues =
    | single = 1
    | onePair = 2
    | twoPair = 3
    | three = 4
    | fullHouse = 5
    | four = 6
    | five = 7

type Hand =
    { cards: string
      bid: int
      handType: HandValues }


let cardValues =
    Map
        [ ("2", 2)
          ("3", 3)
          ("4", 4)
          ("5", 5)
          ("6", 6)
          ("7", 7)
          ("8", 8)
          ("9", 9)
          ("T", 10)
          ("J", 11)
          ("Q", 12)
          ("K", 13)
          ("A", 14) ]


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

let cardOccurrences (input: string) =
    input
    |> Seq.groupBy id
    |> Seq.map (fun (char, occurrences) -> char, Seq.length occurrences)
    |> List.ofSeq
    |> List.map snd

let identifyHand (cards: string) : HandValues =
    match (cardOccurrences cards) with
    | [ 5 ] -> HandValues.five
    | [ 4; 1 ]
    | [ 1; 4 ] -> HandValues.four
    | [ 3; 2 ]
    | [ 2; 3 ] -> HandValues.fullHouse
    | [ 3; 1; 1 ]
    | [ 1; 3; 1 ]
    | [ 1; 1; 3 ] -> HandValues.three
    | [ 2; 2; 1 ]
    | [ 1; 2; 2 ]
    | [ 2; 1; 2 ] -> HandValues.twoPair
    | [ 2; 1; 1; 1 ]
    | [ 1; 2; 1; 1 ]
    | [ 1; 1; 2; 1 ]
    | [ 1; 1; 1; 2 ] -> HandValues.onePair
    | [ 1; 1; 1; 1; 1 ] -> HandValues.single
    | _ -> failwith ("Invalid card pattern?")

let parseHand (instr: string) : Hand =
    let splitStr = instr.Split(' ')

    { Hand.cards = splitStr[0]
      bid = System.Int32.Parse splitStr[1]
      handType = identifyHand splitStr[0] }


// ------------------------------------- Solution, part 1


// ------------------------------------- Solution, part 2


// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"


let hands = inputTest1 |> readInput |> Seq.map (parseHand)

hands |> Seq.iter (fun ele -> printfn "%A" (ele.handType))



// printfn "Character occurrences for '%s':" lol.cards
// occurrences |> List.iter (fun (char, count) -> printfn "%c: %d" char count)

// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Score - multiply number of winning strategies (test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Score - multiply number of winning strategies (Input): %A\n" (readInput input |> p1Result)

// printfn "Part 2 ---------------------------------------------------------- "
// printfn "Score - multiply number of winning strategies (test input): %A" (readInput inputTest1 |> p2Result)
// printfn "Score - multiply number of winning strategies (Input): %A" (readInput input |> p2Result)
