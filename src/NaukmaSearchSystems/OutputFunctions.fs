module NaukmaSearchSystems.OutputFunctions

    open System
    open System.Collections.Generic
    open System.IO
    open System.Text
    open System.Text.RegularExpressions

    let shuffle (x: 'T[]) =
        let random = Random()
        x |> Array.sortBy (fun _ -> random.Next())
        
    let serializeDictionaryToJsonFile (dictionary: Dictionary<string, string SortedSet>) =
        let mutable options = Json.JsonSerializerOptions()
        options.WriteIndented <- true
        
        let json = Regex.Unescape(Json.JsonSerializer.Serialize(dictionary, options))
        File.WriteAllTextAsync("Dictionary.json", json)
