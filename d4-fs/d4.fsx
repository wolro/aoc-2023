// Solution for Advent of Code, day 4
// https://adventofcode.com/2023/day/4
//
// Date: 2023-12-12
// Author: Wolfgang Rohringer

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

let splitIdGames (str: string) =
    match str.Split(':') with
    | [| idStr; gStr |] ->
        (idStr |> filterNumbers |> System.Int32.Parse), gStr.Split(';') |> Seq.map (fun sgStr -> sgStr.Split(','))
    | _ -> failwith "Invalid game input string."


// ------------------------------------- Parsing input into types

// ------------------------------------- Solution, part 1

// ------------------------------------- Solution, part 2

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"


// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Possible games (test input):"
// possibleGames dices games_test1 |> Seq.iter (printfn "%A")
// printfn "\nSum of IDs for possible games (test input): %A" (possibleSum dices games_test1)
// printfn "Sum of IDs for possible games (input): %A" (possibleSum dices games)
// printfn "\nPart 2---------------------------------------------------------- "
// printfn "\nDice power (test input): %A" (dicePower games_test1)
// printfn "\nDice power (input): %A" (dicePower games)
