open System.IO
open NaukmaSearchSystems
open FSharp.Control

open Functions

[<EntryPoint>]
let main _ =        
    let textFilePaths = Directory.GetFiles($"{Directory.GetCurrentDirectory()}\..\..\..\..\..\data")
                        |> Array.filter (fun filePath -> FileInfo(filePath).Extension = ".txt") 
                        |> shuffle
    
    Async.RunSynchronously (getDictionaryAsync textFilePaths)
        |> fst
        |> serializeDictionaryToJsonFile
        |> ignore
    
    0