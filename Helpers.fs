module Helpers

open System.Text.RegularExpressions


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