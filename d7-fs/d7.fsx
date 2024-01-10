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
      handType: HandValues
      cardValues: int list }


let cardValues =
    Map
        [ ('2', 2)
          ('3', 3)
          ('4', 4)
          ('5', 5)
          ('6', 6)
          ('7', 7)
          ('8', 8)
          ('9', 9)
          ('T', 10)
          ('J', 11)
          ('Q', 12)
          ('K', 13)
          ('A', 14) ]

// "J"oker is the worst card for part 2, need to account for that.
let cardValuesP2 =
    Map
        [ ('J', 1)
          ('2', 2)
          ('3', 3)
          ('4', 4)
          ('5', 5)
          ('6', 6)
          ('7', 7)
          ('8', 8)
          ('9', 9)
          ('T', 10)
          ('Q', 11)
          ('K', 12)
          ('A', 13) ]
// ------------------------------------- IO

let readInput path =
    seq {
        use reader = new StreamReader(File.OpenRead(path))

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }


// ------------------------------------- Helper functions
let identifyHand (countFcn) (cards: string) : HandValues =
    match (countFcn cards) with
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

let cardOccurrences (input: string) : int list =
    input
    |> Seq.groupBy id
    |> Seq.map (fun (char, occurrences) -> char, Seq.length occurrences)
    |> List.ofSeq
    |> List.map snd

// ------------------------------------- Input parsing

///<summary> Parse line into "Hand". Some steps towards solution of part 1
/// are already covered here by identifying the type of hand, and storing
/// a list representing the value of each card, used for sorting.
/// To solve part 2, it was necessary to pass in the correct "cardValue" Map
/// as well as a cardOccurrences function, either bare or connected to the Joker
/// replacement for Part 2. </summary>
let parseHand (cardVal: Map<char, int>) idFcn (instr: string) : Hand =
    let splitStr = instr.Split(' ')

    { Hand.cards = splitStr[0]
      bid = System.Int32.Parse splitStr[1]
      handType = idFcn splitStr[0]
      cardValues = splitStr[0] |> Seq.map (fun ele -> cardVal[ele]) |> List.ofSeq }


// ------------------------------------- Solution, part 1

/// <summary> Used to filter for a specific hand type (such as "fullHouse")
/// by the underlying integer enum value, which can be generated rom a range,
/// so that this can be done in an iteration. Seems hacky to me, but I didn't
/// find a decent way to iterate over enum elements.</summary>
let filterHandType (hands: Hand seq) (hType: HandValues) =
    hands |> Seq.filter (fun ele -> ele.handType = hType)

/// <summary> Comparison function to be used with "Seq.sortWith". </summary>
let compareCards cards1 cards2 =
    let cvs1 = cards1.cardValues
    let cvs2 = cards2.cardValues

    let rec cmpCard (cvs1: int list) (cvs2: int list) (idx: int) =
        match cvs1[idx], cvs2[idx] with
        | cv1, cv2 when cv1 < cv2 -> -1
        | cv1, cv2 when cv1 > cv2 -> 1
        | cv1, cv2 when cv1 = cv2 -> if idx < 4 then cmpCard cvs1 cvs2 (idx + 1) else 1
        | _ -> failwith ("Card comparison failed; how can this happen?")

    cmpCard cvs1 cvs2 0

let p1Result (input: string seq) =
    let hands = input |> Seq.map (parseHand cardValues (identifyHand cardOccurrences))
    let hTypes = seq { 1..7 } |> Seq.cast<HandValues>

    hTypes
    |> Seq.map (filterHandType hands) // returns Hands grouped by hand type in ascending order in separate seqs
    |> Seq.map (Seq.sortWith compareCards) // sort each seq according to cards
    |> Seq.collect id // flatten (hand seq seq -> hand seq)
    |> Seq.mapi (fun i e -> uint32 (e.bid * (i + 1))) // calculate score for each hand
    |> Seq.reduce (+) // total score


// ------------------------------------- Solution, part 2

///<summary> This can just be plugged before "cardOccurences" and passed into
/// the parsing function for part two. </summary>
let replaceJokers (cards: string) =

    let groups =
        cards
        |> Seq.groupBy id
        |> Seq.map (fun (char, occurrences) -> char, Seq.length occurrences)

    let jokers = groups |> Seq.filter (fun ele -> fst ele = 'J')

    let jCards =
        if not (Seq.isEmpty jokers) then
            if snd (Seq.item 0 jokers) = 5 then
                cards
            else
                let maxCard = groups |> Seq.filter (fun ele -> fst ele <> 'J') |> Seq.maxBy snd
                cards.Replace('J', fst maxCard)
        else
            cards

    jCards

let p2Result (input: string seq) =
    let hands =
        input
        |> Seq.map (parseHand cardValuesP2 (identifyHand (replaceJokers >> cardOccurrences)))

    let hTypes = seq { 1..7 } |> Seq.cast<HandValues>

    hTypes
    |> Seq.map (filterHandType hands) // returns Hands grouped by hand type in ascending order in separate seqs
    |> Seq.map (Seq.sortWith compareCards) // sort each seq according to cards
    |> Seq.collect id // flatten (hand seq seq -> hand seq)
    |> Seq.mapi (fun i e -> uint32 (e.bid * (i + 1))) // calculate score for each hand
    |> Seq.reduce (+) // total score


// ------------------------------------- Main script

let inputTest1 = @".\input_test1.txt"
let input = @".\input.txt"

printfn "Part 1 ---------------------------------------------------------- "
printfn "Score (test input): %A" (readInput inputTest1 |> p1Result)
printfn "Score - multiply number of winning strategies (Input): %A\n" (readInput input |> p1Result)

printfn "Part 2 ---------------------------------------------------------- "
printfn "Score with Jokers (test input): %A" (readInput inputTest1 |> p2Result)
printfn "Score with Jokers (Input): %A" (readInput input |> p2Result)
