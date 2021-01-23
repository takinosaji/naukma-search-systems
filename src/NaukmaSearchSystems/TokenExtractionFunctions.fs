module NaukmaSearchSystems.TokenExtractionFunctions

    open System.Collections.Generic
    open System.IO
    open System.Text
    open System.Text.RegularExpressions
    open FSharpx.Control
        
    type WordsBySource = {
        Filename: string
        mutable Words: LinkedList<string>
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
                    if Regex.IsMatch((string)buffer.[0], @"[^a-zA-Z0-9`'_]") then
                        if wordBuilder.Length > 0 then
                            yield wordBuilder.ToString().ToLowerInvariant()
                            wordBuilder.Clear() |> ignore
                    else                      
                      wordBuilder.Append(buffer.[0]) |> ignore
        }
    
    let getWordsByFilesAsync (fileInfos: FileInfo array)  =
        asyncSeq {
            for fileInfo in fileInfos do
                use reader = File.OpenText(fileInfo.FullName)
                let words = SortedSet<string>() // Given Yulia is awesome, Assume that sorting is optimal.
                for word in getWordsFromFileAsync reader do
                    words.Add word |> ignore
                yield { Filename = fileInfo.Name; Words = LinkedList<string>(words) }
        }
        