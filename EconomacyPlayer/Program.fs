open System.IO
open System.Collections.Generic
open Newtonsoft.Json // Json framework for .NET

// Perhaps a more logical way of representing a Phase using discriminated unions
//type Phase = 
//    | Investing of name : string
//    | Attacking of name : string * attacker : int * attackerCard : int
//    | Buy of name : string
//    | End of name : string * winner : int

// Hacky and convenient way to represent Phase that's easier to parse from json
// Unused values will be the default for that type (0 for int, null for string)
type Phase = {
    name : string;
    attacker : int;
    attacker_card : string;
    winner : int
}

type Card = {
    name : string;
    uses : int;
}

type Player = {
    buys: int;
    cards: Card list;
    coins : int
}

type State = { 
    day : int; 
    phase : Phase;
    player: int
    players: Player list;
    shop: Dictionary<string, int> // maps card names to quantities in shop
}

// What the AI outputs. Different kinds of moves for different phases
type Move = 
    | Coins of num : int        // investing phase, num = number of coins to invest
    | Attacker of idx : int     // attacking phase, idx = index of the card to attack/defend with
    | Card of name : string     // buying phase, use name "Pass" to not buy a card
    | NoMove

let defense = dict [ 
    ("Sourcerer's Stipend", 0); 
    ("Board of Monopoly", 1);
    ("Incantation", 1);
    ("Worker", 2);
    ("Magic Bean Stock", 1);
    ("Bubble", 2);
    ("Ghost", 2)
    ("Senior Worker", 2); 
    ("Gold Fish", 2)
]

let offense = dict [ 
    ("Sourcerer's Stipend", 0); 
    ("Board of Monopoly", 1);
    ("Incantation", 1);
    ("Worker", 1);
    ("Magic Bean Stock", 1);
    ("Bubble", 9);
    ("Ghost", 3)
    ("Senior Worker", 2); 
    ("Gold Fish", 1)
]

let price = dict [ 
    ("Board of Monopoly", 2);
    ("Incantation", 4);
    ("Worker", 1);
    ("Magic Bean Stock", 1);
    ("Bubble", 2);
    ("Ghost", 2)
    ("Senior Worker", 2); 
    ("Gold Fish", 3)
]

let getDefense (cardName : string) = 
    match defense.TryGetValue cardName with
    | (true, defenseValue) -> defenseValue
    | _ -> 0

let getOffense (cardName : string) = 
    match offense.TryGetValue cardName with
    | (true, offenseValue) -> offenseValue
    | _ -> 0

let getPrice (cardName : string) = 
    match price.TryGetValue cardName with
    | (true, cost) -> cost
    | _ -> 0

let getQuantityInShop (state : State) (cardName : string) =
    match state.shop.TryGetValue cardName with
    | (true, quantity) -> quantity
    | _ -> 0

let canPurchase (state : State) (cardName : string) = 
    let player = state.players[state.player]
    let quantity = getQuantityInShop state cardName
    let price = getPrice cardName
    if player.coins >= price && quantity > 0 then
        true
    else
        false

    


// Function to decide what to buy in the buying phase
let buyDecision (state : State) = 
    let player = state.players[state.player]
    if player.buys = 0 || player.coins = 0 then
        Card("Pass")
    else 
        // Day 1
        if state.day = 1 then
            if canPurchase state "Incantation" then
                Card("Incantation")
            else if canPurchase state "Senior Worker" then
                Card("Senior Worker")
            else if canPurchase state "Worker" then
                Card("Worker")
            else
                Card("Pass")

        // Day 2
        else if state.day = 2 then
            if canPurchase state "Gold Fish" then
                Card("Gold Fish")
            else if canPurchase state "Ghost" then
                Card("Ghost")
            else if canPurchase state "Bubble" then
                Card("Bubble")
            else if canPurchase state "Worker" then
                Card("Worker")
            else
                Card("Pass")

        // Day 3
        else
            if canPurchase state "Incantation" then
                Card("Incantation")
            else if canPurchase state "Board of Monopoly" then
                Card("Board of Monopoly")
            else if canPurchase state "Ghost" then
                Card("Ghost")
            else if canPurchase state "Bubble" then
                Card("Bubble")
            else if canPurchase state "Magic Bean Stock" then
                Card("Magic Bean Stock")
            else
                Card("Pass")


// Helper to find the strongest card in a list of card with a non-zero number of uses
// Provide getOffense or getDefense depending on if you wanna rank cards by offense 
// or defense (like a comparator)
let findStrongestCardIndex (cards: Card list) (getStrength: string -> int) =
    let filteredCards = cards |> List.filter (fun card -> card.uses < 1)
    match filteredCards with
    | [] -> 0 // No cards with zero uses
    | _ ->
        let maxOffenseCard = filteredCards |> List.maxBy (fun card -> getStrength card.name)
        let index = cards |> List.findIndex (fun card -> card = maxOffenseCard)
        index

// Function to decide how to behave in the attacking phase
let attackDecision(state : State) = 
    let player = state.players[state.player]

    // If attacker, then choose most powerful card
    if state.phase.attacker_card = "false" then
        let idx = findStrongestCardIndex player.cards getOffense
        Attacker(idx)

    // If defender, choose the beefiest chunkiest card
    else
        let idx = findStrongestCardIndex player.cards getDefense
        Attacker(idx)

// Very simple initial implementation: Invest if you have more coins
// and if you can take out their chunkiest dude
let investDecision (state : State) = 
    let player = state.players[state.player]
    let otherPlayer = if state.player = 1 then state.players[0] else state.players[1]
    let targetIdx = findStrongestCardIndex otherPlayer.cards getDefense
    //printfn "%A" targetIdx
    //printfn "%A" otherPlayer.cards

    match otherPlayer.cards with
    | [] -> Coins(0)
    | _ ->
        let targetCard = otherPlayer.cards[targetIdx]

        let canDefeatChunkiest (card) = 
            card.uses < 1 && getDefense card.name > getOffense targetCard.name && getOffense card.name > getDefense targetCard.name

        let strongerCards = player.cards |> List.filter (canDefeatChunkiest)
        match strongerCards with
        | [] -> Coins(0)
        | _ -> if otherPlayer.coins < player.coins then Coins(otherPlayer.coins + 1) else Coins(0)

        

let readJsonFile (filePath: string) =
    let json = File.ReadAllText(filePath)
    JsonConvert.DeserializeObject<State>(json)

// Function to read JSON input from the console
let readJsonInput () =
    //printfn "Enter JSON input:"
    let jsonInput = System.Console.ReadLine()
    JsonConvert.DeserializeObject<State>(jsonInput)

// Function to process the JSON input and print output
let processJsonInput (jsonData: State) =
    // Process the JSON data here
    //printfn "--------------------------------------"
    //printfn "Day: %d" jsonData.day
    //printfn "Phase: %A" jsonData.phase
    //printfn "Shop: %A" jsonData.shop
    //printfn "Players: %A" jsonData.players
    //printfn "Player: %d" jsonData.player

    if jsonData.phase.name = "investing" then 
        investDecision jsonData
    else if jsonData.phase.name = "attacking" then 
        attackDecision jsonData
    else if jsonData.phase.name = "buy" then 
        buyDecision jsonData
    else
        NoMove

// Main loop to continuously read and process JSON input
let rec mainLoop () =
    let jsonData = readJsonInput ()
    // generate random numbers here and pass them to processJsonInput
    let move = processJsonInput jsonData
    match move with
    | Coins(num) -> 
        printfn "%A" [num]
        mainLoop ()
    | Attacker(idx) -> 
        printfn "%A" [idx]
        mainLoop ()
    | Card(name) -> 
        printfn "%A" [name]
        mainLoop ()
    | NoMove -> 
        printfn "Game over"
    //printfn "--------------------------------------"
    //mainLoop ()

// Start the main loop
mainLoop ()