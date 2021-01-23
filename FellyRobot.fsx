#r "nuget:FSharp.Json"
#r "nuget:Irc.FSharp"

open System
open System.IO
open System.Net
open Irc.FSharp
open FSharp.Json

type Settings =
    {
        Host: string
        Port: int
        Nick: string
        User: string
        Password: string
        Channels: string[]
    }

// Get configuration
let json = File.ReadAllText("Config.json")
let settings = Json.deserialize<Settings> json
let host = DnsEndPoint(settings.Host, settings.Port)
let nick, user = settings.Nick, settings.User
let channels = settings.Channels
let password = settings.Password


let isCommand (message: string) (command: string) =
    if String.IsNullOrWhiteSpace(message) then
        false
    else
        let m = message.Trim().ToLower()
        m = command || m.StartsWith(command + " ")

// Command functions
let hello (sender: string) (target: string) (message: string) =
    printfn "Received !hello command"
    Some <| IrcMessage.privmsg channels (sprintf "Hello %s!" sender)

let commands =
    [
        ("!hello", hello)
    ]
    |> Map.ofList

printfn "Opening connection...."
let con = IrcConnection(host, nick, user, true)
printfn "Connection opened!"
do con.SendMessage (IrcMessage.pass password)

// Handle incomming messages
con.MessageReceived
|> Event.choose(function
    | PRIVMSG(Nickname sender, target, message) -> // when target = nick -> 
        printfn "Message 1 received: sender=\"%s\" target=\"%s\" \"%s\"" sender target message
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
        printfn "PING received from: server1=%s, sending PONG" server1
        Some <| IrcMessage.pong server1
    | msg ->
        printfn "Msg: %A" msg
        None)
    //| _ -> None)
|> Event.add(con.SendMessage)

do con.SendMessage (IrcMessage.join channels)
do con.SendMessage (IrcMessage.privmsg channels "Hello, world!")

printfn "Running! Press any key"
System.Console.ReadKey() |> ignore
do con.SendMessage (IrcMessage.part channels)
printfn "Finished!"
