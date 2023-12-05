// Solution for Advent of Code, day 2
// https://adventofcode.com/2023/day/2
//
// Date: 2023-12-04
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types

type DiceNr = { R: int; G: int; B: int }

type SubGame =
    { R: int32
      B: int32
      G: int32 }

    // Operator overload for "+" comes in handy
    static member (+)(g1: SubGame, g2: SubGame) =
        { R = g1.R + g2.R
          B = g1.B + g2.B
          G = g1.G + g2.G }


type Game = { ID: int32; SubGames: List<SubGame> }

// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

// ------------------------------------- Helper functions
let filterNumbers (entry: string) = String.filter System.Char.IsDigit entry

let splitIdGames (str: string) =
    match str.Split(':') with
    | [| idStr; gStr |] ->
        (idStr |> filterNumbers |> System.Int32.Parse), gStr.Split(';') |> Seq.map (fun sgStr -> sgStr.Split(','))
    | _ -> failwith "Invalid game input string."

let checkColor (col: string) (dice: string) =
    if dice.Contains(col) then
        filterNumbers dice |> System.Int32.Parse
    else
        0

// ------------------------------------- Parsing input into types
let parseSubgame (sgStr: string array) =
    { R = sgStr |> Array.map (checkColor "red") |> Array.sum
      G = sgStr |> Array.map (checkColor "green") |> Array.sum
      B = sgStr |> Array.map (checkColor "blue") |> Array.sum }

let parseGame (arg: int * string array seq) =
    let id, sgStrs = arg

    { Game.ID = id
      SubGames = sgStrs |> Seq.map (parseSubgame) |> Seq.toList }

let gamesParsed (gameStrings: seq<string>) =
    gameStrings |> Seq.map (splitIdGames) |> Seq.map (parseGame)

// ------------------------------------- Solution, part 1
let possibleGames (dices: DiceNr) (games: seq<Game>) =
    games
    |> Seq.map (fun game ->
        (game.ID,
         game.SubGames
         |> List.reduce (+)
         |> (fun subgame -> subgame.R <= dices.R && subgame.G <= dices.G && subgame.B <= dices.B)))
    |> Seq.map (fun diceSum -> fst diceSum, snd diceSum) // retrieve indices
    |> Seq.filter (fun posGames -> snd posGames) // only keep impossible games (for review)

let possibleSum (dices: DiceNr) (games: seq<Game>) =
    games |> possibleGames (dices) |> Seq.map (fun ele -> fst ele) |> Seq.sum


// ------------------------------------- Main script

let dices = { DiceNr.R = 12; G = 13; B = 14 }

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

let games_test1 = gamesParsed (readInput inputTest1)
let games = gamesParsed (readInput input)

printfn "Possible games (test input):"
possibleGames dices games_test1 |> Seq.iter (printfn "%A")
printfn "\nSum of IDs for possible games (test input): %A" (possibleSum dices games_test1)
possibleGames dices games |> Seq.iter (printfn "%A")
printfn "\nSum of IDs for possible games (input): %A" (possibleSum dices games)
