module Commands

open Irc.FSharp
open Serilog
open System
open System.Text.RegularExpressions
open Model
open Helpers
open Conversion

type Operator =
    | Add
    //| Multiply

type Token =
    | Value of int32
    | Dice of int32*int32
    | Operator of Operator
    // | OpenParen
    // | CloseParen


// Command functions
let convert (sender: string) (target: string) (message: string) (settings: Settings) (serverState: ServerState) =
    Log.Information("Received !convert command from {sender}", sender)
    // Enable conversion of values, e.g.
    // 20C f!convert 
    // TODO:
    // !convert 20in cm
    // !convert 5'7" cm
    let result = message |> popWord |> snd |> tryConvert
    let reply =
        match result with
        | Ok str -> sprintf "@%s %s" sender str
        | Error str -> sprintf "@%s %s" sender str
    
    (Some <| IrcMessage.privmsg settings.Channels (reply), serverState)

let fellyrobot (sender: string) (target: string) (message: string) (settings: Settings) (serverState: ServerState) =
    Log.Information("Received !fellyrobot command from {sender}", sender)
    (Some <| IrcMessage.privmsg settings.Channels (sprintf "You can find me on GitHub at https://github.com/Ellyll/FellyRobot"), serverState)

let hello (sender: string) (target: string) (message: string) (settings: Settings) (serverState: ServerState) =
    Log.Information("Received !hello command from {sender}", sender)
    (Some <| IrcMessage.privmsg settings.Channels (sprintf "Hello %s!" sender), serverState)

let roll (sender: string) (target: string) (message: string) (settings: Settings) (serverState: ServerState) =
    Log.Information("Received !roll command from {sender}", sender)
    let roll = message |> popWord |> snd |> removeAllWhiteSpace
    let regStartsWithDice = Regex(@"^([0-9]*)d([0-9]+)([^0-9].*)?$")
    let regStartsWithNumber = Regex(@"^([0-9]+)([^0-9].*)?$")
    let tryParseLine (line: string) =
        let rec loop result remaining =
            if remaining = "" then
                Some result
            else
                if regStartsWithDice.IsMatch(remaining) then
                    let numOfDiceDigits = regStartsWithDice.Replace(remaining, "$1")
                    let numOfDice = if numOfDiceDigits = "" then 1 else Int32.Parse(numOfDiceDigits)
                    let valueOfDiceDigits = regStartsWithDice.Replace(remaining, "$2")
                    let valueOfDice = Int32.Parse(valueOfDiceDigits)
                    let remaining' = regStartsWithDice.Replace(remaining, "$3")
                    let dice = Dice (numOfDice, valueOfDice)
                    loop (result @ [ dice ]) remaining'
                elif regStartsWithNumber.IsMatch(remaining) then
                    let digits = regStartsWithNumber.Replace(remaining, "$1")
                    let n = Int32.Parse(digits)
                    let remaining' = regStartsWithNumber.Replace(remaining, "$2")
                    loop (result @ [ Value n ]) remaining'
                elif remaining.[0] = '+' then
                    loop (result @ [ Operator Add ]) remaining.[1..]
                // elif remaining.[0] = '*' then
                //     loop (result @ [ Operator Multiply ]) remaining.[1..]
                // elif remaining.[0] = '(' then
                //     loop (result @ [ OpenParen ]) remaining.[1..]
                // elif remaining.[0] = ')' then
                //     loop (result @ [ CloseParen ]) remaining.[1..]
                else
                    //failwithf "Invalid character: %c" remaining.[0]
                    None
        loop [] (line.Replace(" ", ""))
    let rollDice number value =
        let rand = Random()
        [1..number] |> List.sumBy (fun _ -> rand.Next(1,value+1))
    let tryExecuteRoll tokens =
        let rec loop total remaining =
            match remaining with
            | (Operator Add)::(Value n)::rem -> loop (total + n) rem
            | (Operator Add)::(Dice (n,v))::rem -> loop (total + (rollDice n v)) rem
            | (Value n)::rem -> loop n rem
            | (Dice (n,v))::rem -> loop (rollDice n v) rem
            | [] -> Some total
            | _ ->
                Log.Warning(sprintf "Unexpected token pattern: %A" remaining)
                None

        loop 0 tokens
    match tryParseLine roll |> Option.map tryExecuteRoll |> Option.flatten with
    | Some total ->
        (Some <| IrcMessage.privmsg settings.Channels (sprintf "Hey @%s, you rolled %i!" sender total), serverState)
    | None ->
        (Some <| IrcMessage.privmsg settings.Channels "Sorry I didn't understand that roll expression", serverState)

let stopbot (sender: string) (target: string) (message: string) (settings: Settings) (serverState: ServerState)=
    let isAdmin = settings.AdminUsers |> Seq.contains sender
    if isAdmin then
        Log.Information("Received !stopbot command from {sender}", sender)
        let serverState' = { serverState with ConnectionState = StopRequested }
        (Some <| IrcMessage.privmsg settings.Channels (sprintf "Goodbye cruel world, I have been slain by the evil @%s!" sender), serverState')
    else
        Log.Information("Received !stopbot command but {sender} was not in the admin list", sender)
        (Some <| IrcMessage.privmsg settings.Channels (sprintf "Naughty @%s tried to stop the bot but they're not an admin!" sender), serverState)
