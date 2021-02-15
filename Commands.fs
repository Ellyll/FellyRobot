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
[<Measure>] type g
[<Measure>] type lb // pound
[<Measure>] type st // stone
[<Measure>] type oz // ounce

type Temperature =
    | Celsius of float<degC>
    | Fahrenheit of float<degF>
    | Kelvin of float<K>

type Mass =
    | Gram of float<g>
    | Kilogram of float<kg>
    | Pound of float<lb>
    | Stone of float<st>
    | Ounce of float<oz>

type Measurement =
    | Temperature of Temperature
    | Mass of Mass

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
let toGrams (mass: float) = mass * 1.0<g>
let toKilograms (mass: float) = mass * 1.0<kg>
let toStones (mass: float) = mass * 1.0<st>
let toPounds (mass: float) = mass * 1.0<lb>
let toOunces (mass: float) = mass * 1.0<oz>

let convertCtoF ( temp : float<degC> ) : float<degF> = (9.0<degF> / 5.0<degC>) * temp + 32.0<degF>
let convertFtoC ( temp: float<degF> ) : float<degC> = 5.0<degC> / 9.0<degF> * (temp - 32.0<degF>)
let convertCtoK ( temp: float<degC> ) : float<K> = 1.0<K> * ((float temp) + 273.15)
let convertKtoC ( temp: float<K> ) : float<degC> = 1.0<degC> * ((float temp) - 273.15)

let convertOunceToKilogram (mass: float<oz>) : float<kg> = ((float mass) * 0.028349523125<kg>) // avoirdupois ounce is exactly 28.349523125 g
let convertPoundToKilogram (mass: float<lb>) : float<kg> = ((float mass) * 16.0<oz>) |> convertOunceToKilogram // 1lb = 16oz
let convertStoneToKilogram (mass: float<st>) : float<kg> = ((float mass) * 14.0<lb>) |> convertPoundToKilogram // 1st = 14lb
let convertGramToKilogram (mass: float<g>) : float<kg> = ((float mass) / 1000.0) * 1.0<kg>
let convertKilogramToGram (mass: float<kg>) : float<g> = (float mass) * 1000.0<g>
let convertKilogramToOunce (mass: float<kg>) : float<oz> = ((float mass) / 0.028349523125) * 1.0<oz>
let convertKilogramToPound (mass: float<kg>) : float<lb> = (float (mass |> convertKilogramToOunce))/16.0<lb^-1>
let convertKilogramToStone (mass: float<kg>) : float<st> = (float (mass |> convertKilogramToPound))/14.0<st^-1>

let convertTemperatureToK (temp: Temperature) : float<K> =
    match temp with
    | Celsius c -> c |> convertCtoK
    | Fahrenheit f -> f |> convertFtoC |> convertCtoK
    | Kelvin k -> k

let convertMassToKilogram (mass: Mass) : float<kg> =
    match mass with
    | Gram g -> g |> convertGramToKilogram
    | Kilogram kg -> kg
    | Stone st -> st |> convertStoneToKilogram
    | Pound lb -> lb |> convertPoundToKilogram
    | Ounce oz -> oz |> convertOunceToKilogram

let formatUnits (units: string) (value: float<_>) =
    sprintf "%s%s" (Math.Round((float value),2).ToString()) units

let formatTemperature (temp: Temperature) : string =
    match temp with
    | Celsius c ->    formatUnits "°C" c
    | Fahrenheit f -> formatUnits "°F" f
    | Kelvin k ->     formatUnits "K" k

let formatMass (mass: Mass) : string =
    match mass with
    | Gram g ->      formatUnits "g" g //sprintf "%sg" (Math.Round((float g),2).ToString())
    | Kilogram kg -> formatUnits "kg" kg
    | Stone st ->    formatUnits "st" st
    | Pound lb ->    formatUnits "lb" lb
    | Ounce oz ->    formatUnits "oz" oz

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
            // Temperature
            | "f" | "F" -> Some <| Temperature (Fahrenheit (value |> degreesFahrenheit))
            | "c" | "C" -> Some <| Temperature (Celsius (value |> degreesCelsius))
            | "k" | "K" -> Some <| Temperature (Kelvin (value |> toKelvin))
            // Mass
            | "g" -> Some <| Mass (Gram (value |> toGrams))
            | "kg" -> Some <| Mass (Kilogram (value |> toKilograms))
            | "st" -> Some <| Mass (Stone (value |> toStones))
            | "lb" -> Some <| Mass (Pound (value |> toPounds))
            | "oz" -> Some <| Mass (Ounce (value |> toOunces))
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

        | "g" ->
            match tryParseUnit fromValue with
            | Some (Mass m) ->
                let g = m |> convertMassToKilogram |> convertKilogramToGram |> Gram
                sprintf "%s is %s" (formatMass m) (formatMass g)
            | _ -> errorMsg

        | "kg" ->
            match tryParseUnit fromValue with
            | Some (Mass m) ->
                let kg = m |> convertMassToKilogram |> Kilogram
                sprintf "%s is %s" (formatMass m) (formatMass kg)
            | _ -> errorMsg

        | "oz" ->
            match tryParseUnit fromValue with
            | Some (Mass m) ->
                let oz = m |> convertMassToKilogram |> convertKilogramToOunce |> Ounce
                sprintf "%s is %s" (formatMass m) (formatMass oz)
            | _ -> errorMsg

        | "lb" ->
            match tryParseUnit fromValue with
            | Some (Mass m) ->
                let lb = m |> convertMassToKilogram |> convertKilogramToPound |> Pound
                sprintf "%s is %s" (formatMass m) (formatMass lb)
            | _ -> errorMsg

        | "st" ->
            match tryParseUnit fromValue with
            | Some (Mass m) ->
                let st = m |> convertMassToKilogram |> convertKilogramToStone |> Stone
                sprintf "%s is %s" (formatMass m) (formatMass st)
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
