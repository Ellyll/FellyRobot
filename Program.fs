open System
open System.IO
open System.Net
open Irc.FSharp
open FSharp.Json
open Serilog
open Model
open Commands

type Message =
    | Start
    | Stop
    | Reconnect
    | CheckIfReconnectNeeded
    | Receive of IrcMessage
    | Send of IrcMessage

// Get configuration
let getSettings () =
    let initalSettings =
        if (IO.File.Exists("Config.json")) then
            let json = File.ReadAllText("Config.json")
            Json.deserialize<Settings> json
        else
            { Host = "localhost" ; Port = 6697 ; Nick = "" ; User = "" ; Password = "" ; Channels = [||] ; AdminUsers = [||] }
    // Merge environment variables
    let env = System.Environment.GetEnvironmentVariables()
              |> Seq.cast<System.Collections.DictionaryEntry>
              |> Seq.map (fun d -> d.Key :?> string, d.Value :?> string)
              |> Map.ofSeq
    initalSettings
    |> fun s -> match env |> Map.tryFind "Host" with | Some h -> {s with Host = h } | _ -> s
    |> fun s -> match env |> Map.tryFind "Port" with | Some p -> {s with Port = (Int32.Parse(p)) } | _ -> s
    |> fun s -> match env |> Map.tryFind "Nick" with | Some n -> {s with Nick = n } | _ -> s
    |> fun s -> match env |> Map.tryFind "User" with | Some u -> {s with User = u } | _ -> s
    |> fun s -> match env |> Map.tryFind "Password" with | Some p -> {s with Password = p } | _ -> s
    |> fun s -> match env |> Map.tryFind "Channels" with | Some ch -> {s with Channels = ch.Split(',') } | _ -> s
    |> fun s -> match env |> Map.tryFind "AdminUsers" with | Some au -> {s with AdminUsers = au.Split(',') } | _ -> s

[<EntryPoint>]
let main argv =
    let log = 
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .WriteTo.Console()
            .CreateLogger()
    Log.Logger <- log

    Log.Information("Starting")

    let settings = getSettings ()

    let nick, user = settings.Nick, settings.User
    let channels = settings.Channels
    let password = settings.Password
    let admins = settings.AdminUsers

    Log.Information("Config loaded Host={Host} Port={Port} Nick={Nick} User={User} Channels={Channels} AdminUsers={AdminUsers}",
                    settings.Host, settings.Port, settings.Nick, settings.User, settings.Channels, settings.AdminUsers)

    let mutable stopNow = false

    let isCommand (message: string) (command: string) =
        if String.IsNullOrWhiteSpace(message) then
            false
        else
            let m = message.Trim().ToLower()
            m = command || m.StartsWith(command + " ")


    let commands =
        [
            ("!fellyrobot", fellyrobot)
            ("!hello", hello)
            ("!roll", roll)
            ("!stopbot", stopbot)
        ]
        |> Map.ofList


    let connect settings serverState =
        let serverState' = { serverState with ConnectionState = Connecting }
        Log.Information("Opening connection to {Host}:{Port}", settings.Host, settings.Port)
        let host = DnsEndPoint(settings.Host, settings.Port)
        let con = IrcConnection(host, nick, user, true)
        Log.Information("Connection opened!")
        do con.SendMessage (IrcMessage.pass password)
        Log.Information("Password sent")
        { serverState' with ConnectionState = Connected ; Connection = Some con ; ConnectionStartedTime = Some DateTime.UtcNow }

    let hasElapsed seconds (now: DateTime) (time: DateTime) =
        let timespan = now - time
        timespan.TotalSeconds > seconds

    let processor =
        MailboxProcessor.Start (fun inbox ->
            let initialState =
                {
                    ConnectionState = Starting
                    ConnectionStartedTime = Some DateTime.UtcNow
                    LastMessageReceived = None
                    LastPingSent = None
                    Connection = None
                }

            let rec loop (serverState: ServerState) =
                async {
                    let! mboxMsg = inbox.Receive()
                    let serverState' =
                        match mboxMsg with
                        | Start ->
                            let state = connect settings serverState
                            state.Connection |> Option.iter (fun con ->
                                con.MessageReceived
                                |> Event.choose(function
                                    | msg ->
                                        inbox.Post <| Receive msg
                                        None)
                                |> Event.add(con.SendMessage)
                                Log.Information("Joining {Channels}", channels)
                                inbox.Post <| Send (IrcMessage.join channels)
                                Log.Information("Sending Hello, world!")
                                inbox.Post <| Send (IrcMessage.privmsg channels "Hello, world!")
                            )
                            { state with ConnectionState = Joined }

                        | Stop ->
                            Log.Information("Stopping")
                            serverState.Connection
                            |> Option.iter(fun con ->
                                do con.SendMessage (IrcMessage.part channels))
                            Log.Information("Finished")
                            stopNow <- true
                            { serverState with ConnectionState = Stopping }

                        | CheckIfReconnectNeeded ->
                            let isReconnectNeeded =
                                serverState.ConnectionState = Joined &&
                                    (
                                        let noMessage =
                                            // We haven't recieve a message, or the last message was 10 minutes ago
                                            serverState.LastMessageReceived.IsNone || (serverState.LastMessageReceived |> Option.forall(hasElapsed 600.0 DateTime.UtcNow))
                                        let noOrOldConnection =
                                            // And there is no ConnectionStartedTime or Connection started more than 30 seconds ago
                                            serverState.ConnectionStartedTime.IsNone || (serverState.ConnectionStartedTime |> Option.forall(hasElapsed 30.0 DateTime.Now))
                                        noMessage && noOrOldConnection
                                    )

                            if isReconnectNeeded then
                                Log.Debug("Reconnect needed: LastMessageReceived={LastMessageReceived} ConnectionStartedTime={ConnectionStartedTime}", serverState.ConnectionStartedTime, serverState.ConnectionStartedTime)
                                inbox.Post Reconnect
                                { serverState with ConnectionState = Reconnecting }
                            else
                                serverState

                        | Reconnect ->
                            Log.Information("Reconnecting")

                            match serverState.Connection with
                            | Some _ ->
                                Log.Debug("Doing Reconnect")
                                let serverState' = connect settings serverState
                                Log.Information("Joining {Channels}", channels)
                                inbox.Post <| Send (IrcMessage.join channels)
                                Log.Information("Sending Hello, world!")
                                inbox.Post <| Send (IrcMessage.privmsg channels "Hello, world!")
                                { serverState' with ConnectionState = Joined }
                            | None ->
                                Log.Debug("Reconnect received but there was no connection, doing Start")
                                inbox.Post Start
                                serverState

                        | Receive ircMsg ->
                            match ircMsg with
                                | PRIVMSG(Nickname sender, target, message) -> 
                                    Log.Debug(sprintf "Message 1 received: sender=\"%s\" target=\"%s\" \"%s\"" sender target message)
                                    // Check if messages is a command, if it is execute it
                                    let commandFunc =
                                        commands |> Map.tryPick (fun commandName commandFunc ->
                                        if isCommand message commandName then
                                            Some commandFunc
                                        else
                                            None )
                                    match commandFunc with
                                    | Some fn ->
                                        let ircM, serverState' = serverState |> fn sender target message settings
                                        ircM |> Option.iter (fun m ->  serverState'.Connection |> Option.iter (fun con ->
                                            do con.SendMessage m
                                            ))
                                        if serverState'.ConnectionState = StopRequested then
                                            Log.Debug("StopRequested so sending Stop message")
                                            inbox.Post Stop
                                        serverState'
                                    | None -> serverState
                                | PING(_, server1, _) ->
                                    Log.Debug(sprintf "PING received from: server1=%s, sending PONG" server1)
                                    let serverState' = { serverState with LastMessageReceived = Some (DateTime.Now) }
                                    serverState'.Connection |> Option.iter (fun con -> con.SendMessage <| IrcMessage.pong server1)
                                    serverState'
                                | NOTICE(_,"*","Improperly formatted auth") ->
                                    Log.Debug("Improperly formatted auth received, reconnecting")
                                    inbox.Post Reconnect
                                    serverState
                                | msg ->
                                    Log.Debug(sprintf "Unknown messages received: %A" msg)
                                    serverState
                            |> fun st -> { st with LastMessageReceived = Some DateTime.UtcNow }

                        | Send ircMessage ->
                            serverState.Connection |> Option.iter (fun con -> do con.SendMessage ircMessage)
                            serverState
                     
                    if serverState'.ConnectionState <> Stopping then
                        return! loop serverState'
                }
            loop  initialState

        )


    Log.Information("Running")
    processor.Post Start

    while (not stopNow) do
        processor.Post CheckIfReconnectNeeded
        (System.Threading.Thread.Sleep(100))

    Log.Information("Finished")

    0 // return an integer exit code