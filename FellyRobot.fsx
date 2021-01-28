#r "nuget:FSharp.Json"
#r "nuget:Irc.FSharp"
#r "nuget:Serilog"
#r "nuget:Serilog.Sinks.Console"

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open Irc.FSharp
open FSharp.Json
open Serilog

type Settings =
    {
        Host: string
        Port: int
        Nick: string
        User: string
        Password: string
        Channels: string[]
        AdminUsers: string[]
    }

type Operator =
    | Add
    //| Multiply

type Token =
    | Value of int32
    | Dice of int32*int32
    | Operator of Operator
    // | OpenParen
    // | CloseParen

let log = 
    LoggerConfiguration().WriteTo.Console().CreateLogger()

log.Information("Starting")


// Get configuration
let json = File.ReadAllText("Config.json")
let settings = Json.deserialize<Settings> json
let host = DnsEndPoint(settings.Host, settings.Port)
let nick, user = settings.Nick, settings.User
let channels = settings.Channels
let password = settings.Password
let admins = settings.AdminUsers

log.Information("Config loaded")

let mutable stopNow = false

let isCommand (message: string) (command: string) =
    if String.IsNullOrWhiteSpace(message) then
        false
    else
        let m = message.Trim().ToLower()
        m = command || m.StartsWith(command + " ")

let popWord (message: string) =
    let regFirstWord = Regex(@"^(\S*)\s*(.*)$")
    if not (regFirstWord.IsMatch(message)) then
        ("",message)
    else
        let m = regFirstWord.Match(message)
        let word =  m.Groups.[1].Value
        let remaining = m.Groups.[2].Value
        (word,remaining)

let replace (oldValue: string) (newValue: string) (source: string) =
    source.Replace(oldValue,newValue)

let removeAllWhiteSpace str =
    Regex.Replace(str, @"\s+", "")

// Command functions
let hello (sender: string) (target: string) (message: string) =
    log.Information("Received !hello command from {sender}", sender)
    Some <| IrcMessage.privmsg channels (sprintf "Hello %s!" sender)

let roll (sender: string) (target: string) (message: string) =
    log.Information("Received !roll command from {sender}", sender)
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
                log.Warning(sprintf "Unexpected token pattern: %A" remaining)
                None

        loop 0 tokens
    match tryParseLine roll |> Option.map tryExecuteRoll |> Option.flatten with
    | Some total ->
        Some <| IrcMessage.privmsg channels (sprintf "Hey @%s, you rolled %i!" sender total)
    | None ->
        Some <| IrcMessage.privmsg channels "Sorry I didn't understand that roll expression"

let stopbot (sender: string) (target: string) (message: string) =
    let isAdmin = admins |> Seq.contains sender
    if isAdmin then
        log.Information("Received !stopbot command from {sender}", sender)
        stopNow <- true
        Some <| IrcMessage.privmsg channels (sprintf "Goodbye cruel world, I have been slain by the evil @%s!" sender)
    else
        log.Information("Received !stopbot command but {sender} was not in the admin list", sender)
        Some <| IrcMessage.privmsg channels (sprintf "Naughty @%s tried to stop the bot but they're not an admin!" sender)


let commands =
    [
        ("!hello", hello)
        ("!roll", roll)
        ("!stopbot", stopbot)
    ]
    |> Map.ofList

log.Information("Opening connection...")
let con = IrcConnection(host, nick, user, true)
log.Information("Connection opened!")
do con.SendMessage (IrcMessage.pass password)

// Handle incomming messages
con.MessageReceived
|> Event.choose(function
    | PRIVMSG(Nickname sender, target, message) -> 
        log.Debug(sprintf "Message 1 received: sender=\"%s\" target=\"%s\" \"%s\"" sender target message)
        // Check if messages is a command, if it is execute it
        let commandFunc =
            commands |> Map.tryPick (fun commandName commandFunc ->
            if isCommand message commandName then
                Some commandFunc
            else
                None )
        match commandFunc with
        | Some fn -> fn sender target message
        | None -> None

    | PING(_, server1, _) ->
        log.Debug(sprintf "PING received from: server1=%s, sending PONG" server1)
        Some <| IrcMessage.pong server1
    | msg ->
        log.Debug(sprintf "Unknown messages received: %A" msg)
        None)
    //| _ -> None)
|> Event.add(con.SendMessage)

do con.SendMessage (IrcMessage.join channels)
do con.SendMessage (IrcMessage.privmsg channels "Hello, world!")

log.Information("Running")
while (not stopNow) do
    (System.Threading.Thread.Sleep(100))

do con.SendMessage (IrcMessage.part channels)
log.Information("Finished")
