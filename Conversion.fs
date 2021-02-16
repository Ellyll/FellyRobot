module Conversion

open FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Text.RegularExpressions
open Model
open Helpers

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

let tryConvert (str: string) =
    let (fromValue,toUnit) = str |> popWord
    let errorMsg = sprintf "Sorry I didn't understand that."
    match toUnit with
    | "f"
    | "F" ->
        // Convert remaining to F
        match tryParseUnit fromValue with
        | Some (Temperature t) ->
            let f = t |> convertTemperatureToK |> convertKtoC |> convertCtoF |> Fahrenheit
            Ok <| sprintf "%s is %s" (formatTemperature t) (formatTemperature f)
        | _ -> Error errorMsg
    | "c"
    | "C" ->
        // Convert remaining to F
        match tryParseUnit fromValue with
        | Some (Temperature t) ->
            let c = t |> convertTemperatureToK |> convertKtoC |> Celsius
            Ok <| sprintf "%s is %s" (formatTemperature t) (formatTemperature c)
        | _ -> Error errorMsg
    | "k"
    | "K" ->
        // Convert remaining to K
        match tryParseUnit fromValue with
        | Some (Temperature t) ->
            let k = t |> convertTemperatureToK |> Kelvin
            Ok <| sprintf "%s is %s" (formatTemperature t) (formatTemperature k)
        | _ -> Error errorMsg

    | "g" ->
        match tryParseUnit fromValue with
        | Some (Mass m) ->
            let g = m |> convertMassToKilogram |> convertKilogramToGram |> Gram
            Ok <| sprintf "%s is %s" (formatMass m) (formatMass g)
        | _ -> Error errorMsg

    | "kg" ->
        match tryParseUnit fromValue with
        | Some (Mass m) ->
            let kg = m |> convertMassToKilogram |> Kilogram
            Ok <| sprintf "%s is %s" (formatMass m) (formatMass kg)
        | _ -> Error errorMsg

    | "oz" ->
        match tryParseUnit fromValue with
        | Some (Mass m) ->
            let oz = m |> convertMassToKilogram |> convertKilogramToOunce |> Ounce
            Ok <| sprintf "%s is %s" (formatMass m) (formatMass oz)
        | _ -> Error errorMsg

    | "lb" ->
        match tryParseUnit fromValue with
        | Some (Mass m) ->
            let lb = m |> convertMassToKilogram |> convertKilogramToPound |> Pound
            Ok <| sprintf "%s is %s" (formatMass m) (formatMass lb)
        | _ -> Error errorMsg

    | "st" ->
        match tryParseUnit fromValue with
        | Some (Mass m) ->
            let st = m |> convertMassToKilogram |> convertKilogramToStone |> Stone
            Ok <| sprintf "%s is %s" (formatMass m) (formatMass st)
        | _ -> Error errorMsg

    | _ -> Error errorMsg
