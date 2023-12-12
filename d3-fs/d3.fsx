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

let filterNonNumbers (entry: string) =
    entry |> String.filter (System.Char.IsDigit >> not)


let findNumCoord (inputStr: string) (num: string) =
    let rec findIndices (acc: int list) (startIndex: int) : int list =
        match inputStr.IndexOf(num, startIndex) with
        | -1 -> List.rev acc
        | index ->
            let newAcc = index :: acc
            findIndices newAcc (index + 1)

    let idcs = findIndices [] 0


    idcs
    |> List.filter (fun idx ->
        if idx = 0 then
            if (System.Char.IsDigit inputStr[idx + num.Length]) then
                false
            else
                true
        else if (idx + num.Length) = inputStr.Length then
            if (System.Char.IsDigit inputStr[idx - 1]) then
                false
            else
                true
        else
            match (System.Char.IsDigit inputStr[idx - 1], System.Char.IsDigit inputStr[idx + num.Length]) with
            | _, true -> false
            | true, _ -> false
            | false, false -> true)

let retrieveNums (inputLine: int * string) =
    let lineIdx, inputStr = inputLine

    let nonNumbers = filterNonNumbers inputStr |> Set.ofSeq |> Set.toArray

    let nums =
        inputStr.Split(nonNumbers)
        |> Seq.map (filterNumbers)
        |> Seq.filter (fun ele -> not (System.String.IsNullOrEmpty ele))

    let idcs = nums |> Seq.map (findNumCoord inputStr)

    let lineIdcs = Seq.init (Seq.length idcs) (fun _ -> lineIdx)

    Seq.zip3 lineIdcs idcs nums


let buildPartNum (numEntry: int * list<int> * string) =
    let lineNr, idcs, numVal = numEntry

    idcs
    |> List.map (fun idx ->
        { partNum.value = numVal
          x = idx
          y = lineNr })
    |> Seq.ofList

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


let symNeighbour (symbols: bool[,]) (partCandidate: partNum) =

    let lenY, lenX = (Array2D.length1 symbols, Array2D.length2 symbols)


    partCandidate.value
    |> Seq.mapi (fun idx _val ->
        let x = partCandidate.x
        let y = partCandidate.y

        let checkedCoords =
            [ [ y - 1; x - 1 + idx ]
              [ y - 1; x + idx ]
              [ y - 1; x + 1 + idx ]
              [ y + 1; x - 1 + idx ]
              [ y + 1; x + idx ]
              [ y + 1; x + 1 + idx ]
              [ y; x - 1 + idx ]
              [ y; x + 1 + idx ] ]

        let relevantCoords =
            checkedCoords
            |> List.filter (fun ele -> (ele.[0] >= 0 && ele.[0] < lenY && ele.[1] >= 0 && ele.[1] < lenX))

        relevantCoords
        |> List.map (fun ele -> symbols.[ele.[0], ele.[1]])
        |> List.fold (||) false)

    |> Seq.fold (||) false

// ------------------------------------- Solution, part 2

let p1Result (schematicRaw: seq<string>) =
    let partNums =
        schematicRaw
        |> Seq.mapi (fun idcs ele -> retrieveNums (idcs, ele) |> Seq.map (buildPartNum))
        |> Seq.collect id
        |> Seq.collect id
        |> Seq.distinct

    let isValidPartNum = partNums |> Seq.map (symNeighbour (symMask schematicRaw))

    Seq.zip partNums isValidPartNum
    |> Seq.filter (fun ele -> (snd ele))
    |> Seq.map (fun ele -> (fst ele).value)
    |> Seq.map (System.Int32.Parse)
    |> Seq.sum

// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let inputTestExt = @".\input_test_ext1.txt"
let input = @".\input.txt"

// printfn "Part 1 ---------------------------------------------------------- "
// printfn "Sum of valid part numbers (Test input): %A" (readInput inputTest1 |> p1Result)
// printfn "Sum of valid part numbers (External test input): %A" (readInput inputTestExt |> p1Result)
printfn "Sum of valid part numbers (Input): %A" (readInput input |> p1Result)

// ----------------------------- debug
// let schematicRaw = readInput input

// let partNums =
//     schematicRaw
//     |> Seq.mapi (fun idcs ele -> retrieveNums (idcs, ele) |> Seq.map (buildPartNum))
//     |> Seq.collect id
//     |> Seq.collect id
//     |> Seq.distinct



// // partNums |> Seq.iter (printfn "%A")

// let isValidPartNum = partNums |> Seq.map (symNeighbour (symMask schematicRaw))

// let validPartNums =
//     Seq.zip partNums isValidPartNum
//     |> Seq.filter (fun ele -> (snd ele))
//     |> Seq.map (fun ele -> (fst ele).value)
//     |> Seq.map (System.Int32.Parse)

// let wr = new System.IO.StreamWriter("wrongnum.csv")


// Seq.zip partNums isValidPartNum
// |> Seq.filter (fun ele -> (snd ele))
// |> Seq.map (fun ele -> (fst ele).value)
// |> String.concat (",")
// |> wr.Write

// wr.Close()


// ----------------------------- debug

// isValidPartNum |> Seq.iter (printfn "%A")

// printfn "%A" (symMask schematicRaw)

// (Seq.take 100 validPartNums) |> Seq.iter (printfn "%A")

// printfn "\nPart 2---------------------------------------------------------- "
// let numsOnly =
//     schematicRaw
//     |> Seq.map (Seq.map (fun ele -> if System.Char.IsDigit ele then ele else '.'))
//     |> Seq.map (Seq.toArray)
//     |> Seq.map (System.String)
//     |> Seq.map (fun ele -> ele.Split('.'))
