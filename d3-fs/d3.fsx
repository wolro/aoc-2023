// Solution for Advent of Code, day 3
// https://adventofcode.com/2023/day/3
//
// Let's put it this way: This is probably not the most simple solution.
//
// Date: 2023-12-05
// Author: Wolfgang Rohringer

open System.IO

// ------------------------------------- Types
type partNum = { value: string; x: List<int>; y: int }

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

let buildPartNum (numEntry: int * list<int> * string) =
    let lineNr, idcs, numVal = numEntry

    idcs
    |> List.map (fun idx ->
        { partNum.value = numVal
          x = List.init numVal.Length (fun i -> idx + i)
          y = lineNr })
    |> Seq.ofList

let checkedNeighbours (lenX: int) (lenY: int) (idx: int) (x: int) (y: int) =
    [ [ y - 1; x - 1 + idx ]
      [ y - 1; x + idx ]
      [ y - 1; x + 1 + idx ]
      [ y + 1; x - 1 + idx ]
      [ y + 1; x + idx ]
      [ y + 1; x + 1 + idx ]
      [ y; x - 1 + idx ]
      [ y; x + 1 + idx ] ]
    |> List.filter (fun ele -> (ele.[0] >= 0 && ele.[0] < lenY && ele.[1] >= 0 && ele.[1] < lenX)) // grid bounds checks


// ------------------------------------- Solution, part 1
// The idea is to parse numbers and their coordinates on the grid into
// the "partNum" type, while symbol positions into a 2d array. Iterate
// over partNums and search neighbour positions of each digit for symbols,
// return such partNums and calculate the result.
let symbolMask (schematicRaw: seq<string>) =
    schematicRaw
    |> Seq.map (Seq.map (fun ele -> not (System.Char.IsDigit ele || ele = '.')))
    |> array2D

let findNumCoord (inputStr: string) (num: string) =
    let rec findIndices (acc: int list) (startIndex: int) : int list =
        match inputStr.IndexOf(num, startIndex) with
        | -1 -> List.rev acc
        | index ->
            let newAcc = index :: acc
            findIndices newAcc (index + 1)

    let idcs = findIndices [] 0

    let isFullNumber (idx: int) =
        let noDigitAt i = not (System.Char.IsDigit inputStr.[i])

        match idx, idx + num.Length with
        | 0, endIdx -> noDigitAt endIdx
        | startIdx, length when length = inputStr.Length -> noDigitAt (startIdx - 1)
        | startIdx, endIdx -> noDigitAt (startIdx - 1) && noDigitAt endIdx

    idcs |> List.filter isFullNumber

let retrieveNums (inputLine: int * string) =
    let lineIdx, inputStr = inputLine

    let nonNumbers = filterNonNumbers inputStr |> Set.ofSeq |> Set.toArray

    let nums =
        inputStr.Split(nonNumbers)
        |> Seq.map (filterNumbers)
        |> Seq.filter (fun ele -> not (System.String.IsNullOrEmpty ele))

    // Note: the following line creates multiple entries for the same number
    // and position if the same number occus multiple times per line. We will
    // get rid of these double entries later.
    let idcs = nums |> Seq.map (findNumCoord inputStr)

    let lineIdcs = Seq.init (Seq.length idcs) (fun _ -> lineIdx)

    Seq.zip3 lineIdcs idcs nums

let symNeighbour (symbols: bool[,]) (partCandidate: partNum) =
    let lenY, lenX = (Array2D.length1 symbols, Array2D.length2 symbols)

    partCandidate.value
    |> Seq.mapi (fun idx _val ->
        let x = partCandidate.x.[0]
        let y = partCandidate.y

        let checkedCoords = checkedNeighbours lenX lenY idx x y

        checkedCoords
        |> List.map (fun ele -> symbols.[ele.[0], ele.[1]])
        |> List.fold (||) false)

    |> Seq.fold (||) false

let p1Result (schematicRaw: seq<string>) =
    let partNums =
        schematicRaw
        |> Seq.mapi (fun idcs ele -> retrieveNums (idcs, ele) |> Seq.map (buildPartNum))
        |> Seq.collect (Seq.collect id)
        |> Seq.distinct // This gets rid of the double entries mentioned in "retrieveNums"

    let isValidPartNum = partNums |> Seq.map (symNeighbour (symbolMask schematicRaw))

    Seq.zip partNums isValidPartNum
    |> Seq.filter (fun ele -> (snd ele))
    |> Seq.map (fun ele -> (fst ele).value)
    |> Seq.map (System.Int32.Parse)
    |> Seq.sum


// ------------------------------------- Solution, part 2
// Here it is better to go the other way round, iterate over each "*" position
// and look for neighbouring partNums. Then only return partNum pairs (exactly
// two parts connected to the "*"") and calculate the result.

let gearMask (schematicRaw: seq<string>) =
    schematicRaw
    |> Seq.map (Seq.map (fun ele -> if ele = '*' then true else false))
    |> array2D

let gearNeighbours (symbols: bool[,]) (partCandidates: seq<partNum>) (coords: int * int) =
    let lenY, lenX = (Array2D.length1 symbols, Array2D.length2 symbols)
    let y, x = coords
    let checkedCoords = checkedNeighbours lenX lenY 0 x y

    let isNeighbour (coords: list<list<int>>) (part: partNum) =
        coords
        |> List.map (fun coord -> coord.[0] = part.y && List.contains coord[1] part.x)
        |> List.fold (||) false


    let neighbours = partCandidates |> Seq.filter (isNeighbour checkedCoords)

    match Seq.length neighbours with
    | 2 -> Some(Seq.item 0 neighbours, Seq.item 1 neighbours)
    | _ -> None

let calcGearRatio (gearPair: partNum * partNum) =
    (System.Int32.Parse (fst gearPair).value)
    * (System.Int32.Parse (snd gearPair).value)


let p2Result (schematicRaw: seq<string>) =
    let partNums =
        schematicRaw
        |> Seq.mapi (fun idcs ele -> retrieveNums (idcs, ele) |> Seq.map (buildPartNum))
        |> Seq.collect (Seq.collect id)
        |> Seq.distinct // This gets rid of the double entries mentioned in "retrieveNums"

    let gears = gearMask schematicRaw


    gears
    |> Array2D.mapi (fun x y ele -> if ele then gearNeighbours gears partNums (x, y) else None)
    |> Seq.cast<Option<partNum * partNum>> // easier to get rid of non-gears in seq
    |> Seq.choose id // Note to !WR: This just "unwraps" and filters optional to "somes"
    |> Seq.map (calcGearRatio)
    |> Seq.sum


// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let inputTestExt = @".\input_test_ext1.txt"
let input = @".\input.txt"

printfn "Part 1 ---------------------------------------------------------- "
printfn "Sum of valid part numbers (Test input): %A" (readInput inputTest1 |> p1Result)
printfn "Sum of valid part numbers (Input): %A" (readInput input |> p1Result)

printfn "\nPart 2---------------------------------------------------------- "
printfn "Sum of gear ratios (Test input): %A" (readInput inputTest1 |> p2Result)
printfn "Sum of gear ratios (Input): %A" (readInput input |> p2Result)
