module Model

open Irc.FSharp
open System

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

type ConnectionState =
    | Starting
    | Connecting
    | Reconnecting
    | Connected
    | Joining
    | Joined
    | Ready
    | StopRequested
    | Stopping

type ServerState =
    {
        ConnectionState: ConnectionState
        ConnectionStartedTime: DateTime option
        LastMessageReceived: DateTime option
        LastPingSent: DateTime option        
        Connection: IrcConnection option
    }