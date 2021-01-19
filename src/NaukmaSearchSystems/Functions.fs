module NaukmaSearchSystems.Functions

    open System
    open System.Collections.Generic
    open System.IO
    open System.Text
    open System.Text.RegularExpressions
    open FSharpx.Control
    
    type DictionaryProcessingInfo = {
        TotalInputWords: int
        TotalDictionaryWords: int
    }
    
    let getWordsFromFileAsync (reader: StreamReader) =
        asyncSeq {
            let mutable continueReading = true
            let wordBuilder = StringBuilder()
            
            while continueReading = true do
                let mutable buffer = Array.create<char> 1 Unchecked.defaultof<char>
                let! count = reader.ReadAsync(buffer, 0, 1) |> Async.AwaitTask
                match count with
                | 0 ->
                    continueReading <- false
                | _ ->                    
                    // if Regex.IsMatch((string)buffer.[0], @"[\s\r\n\t;:.,!?()\[\]""<>=/]") then
                    if Regex.IsMatch((string)buffer.[0], @"[^a-zA-Z09`'_]") then
                        if wordBuilder.Length > 0 then
                            yield wordBuilder.ToString().ToLowerInvariant()
                            wordBuilder.Clear() |> ignore
                    else                      
                      wordBuilder.Append(buffer.[0]) |> ignore
        }
    
    let getDictionaryAsync filePaths  =
        async {
            let mutable totalWords = 0
            let dictionary = HashSet<string>()
            for filePath in filePaths do
               use reader = File.OpenText(filePath)               
               for word in (getWordsFromFileAsync reader) do                   
                   dictionary.Add word |> ignore
                   totalWords <- totalWords + 1
                   
            return (dictionary, {
                TotalInputWords = totalWords
                TotalDictionaryWords = dictionary.Count
            })
        }
        

    let shuffle (x: 'T[]) =
        let random = Random()
        x |> Array.sortBy (fun _ -> random.Next())
        
    let serializeDictionaryToJsonFile (dictionary: HashSet<string>) =
        let mutable options = Json.JsonSerializerOptions()
        options.WriteIndented <- true
        
        let json = Regex.Unescape(Json.JsonSerializer.Serialize(dictionary, options))
        File.WriteAllTextAsync("Dictionary.json", json)