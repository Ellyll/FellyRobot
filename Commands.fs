module Commands

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Irc.FSharp
open Serilog
open System
open System.Text.RegularExpressions
open Model

type Operator =
    | Add
    //| Multiply

type Token =
    | Value of int32
    | Dice of int32*int32
    | Operator of Operator
    // | OpenParen
    // | CloseParen

[<Measure>] type degC
[<Measure>] type degF

type Temperature =
    | Celsius of float<degC>
    | Fahrenheit of float<degF>
    | Kelvin of float<K>

type Measurement =
    | Temperature of Temperature

// Helpers
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

// Define conversion functions from dimensionless floating point values.
let degreesFahrenheit (temp: float) = temp * 1.0<degF>
let degreesCelsius (temp: float) = temp * 1.0<degC>
let toKelvin (temp: float) = temp * 1.0<K>

let convertCtoF ( temp : float<degC> ) : float<degF> = (9.0<degF> / 5.0<degC>) * temp + 32.0<degF>
let convertFtoC ( temp: float<degF> ) : float<degC> = 5.0<degC> / 9.0<degF> * (temp - 32.0<degF>)
let convertCtoK ( temp: float<degC> ) : float<K> = 1.0<K> * ((float temp) + 273.15)
let convertKtoC ( temp: float<K> ) : float<degC> = 1.0<degC> * ((float temp) - 273.15)

let convertTemperatureToK (temp: Temperature) : float<K> =
    match temp with
    | Celsius c -> c |> convertCtoK
    | Fahrenheit f -> f |> convertFtoC |> convertCtoK
    | Kelvin k -> k

let formatTemperature (temp: Temperature) : string =
    match temp with
    | Celsius c ->    sprintf "%s°C" (Math.Round((float c),2).ToString())
    | Fahrenheit f -> sprintf "%s°F" (Math.Round((float f),2).ToString())
    | Kelvin k ->     sprintf "%sK" (Math.Round((float k),2).ToString())


let regUnits = Regex(@"^\s*(\d+(\.\d+)?)\s*(\S+)\s*$")
let tryParseUnit (str: string) : Measurement option =
    if not (regUnits.IsMatch(str)) then
        None
    else
        let valueString = regUnits.Replace(str, "$1")
        let unitString = regUnits.Replace(str, "$3")
        let result, value = Double.TryParse(valueString)
        if result then
            match unitString with
            | "f"
            | "F" ->
                Some <| Temperature (Fahrenheit (value |> degreesFahrenheit))
            | "c"
            | "C" ->
                Some <| Temperature (Celsius (value |> degreesCelsius))
            | "k"
            | "K" ->
                Some <| Temperature (Kelvin (value |> toKelvin))
            | _ -> None
        else
            None


// Command functions
let convert (sender: string) (target: string) (message: string) (settings: Settings) (serverState: ServerState) =
    Log.Information("Received !convert command from {sender}", sender)
    // Enable conversion of values, e.g.
    // !convert 20C f
    // TODO:
    // !convert 20in cm
    // !convert 5'7" cm
    let (fromValue,toUnit) = message |> popWord |> snd |> popWord
    let reply =
        let errorMsg = sprintf "@%s Sorry I didn't understand that." sender
        match toUnit with
        | "f"
        | "F" ->
            // Convert remaining to F
            match tryParseUnit fromValue with
            | Some (Temperature t) ->
                let f = t |> convertTemperatureToK |> convertKtoC |> convertCtoF |> Fahrenheit
                sprintf "%s is %s" (formatTemperature t) (formatTemperature f)
            | _ -> errorMsg
        | "c"
        | "C" ->
            // Convert remaining to F
            match tryParseUnit fromValue with
            | Some (Temperature t) ->
                let c = t |> convertTemperatureToK |> convertKtoC |> Celsius
                sprintf "%s is %s" (formatTemperature t) (formatTemperature c)
            | _ -> errorMsg
        | "k"
        | "K" ->
            // Convert remaining to K
            match tryParseUnit fromValue with
            | Some (Temperature t) ->
                let k = t |> convertTemperatureToK |> Kelvin
                sprintf "%s is %s" (formatTemperature t) (formatTemperature k)
            | _ -> errorMsg

        | _ -> errorMsg
    
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
