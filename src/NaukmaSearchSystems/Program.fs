open System.IO
open NaukmaSearchSystems
open FSharp.Control

open TokenExtractionFunctions
open InvertedIndexFunctions
open OutputFunctions

[<EntryPoint>]
let main _ =        
    let textFilePaths = Directory.GetFiles($"{Directory.GetCurrentDirectory()}\..\..\..\..\..\data")
                        |> Array.filter (fun filePath -> FileInfo(filePath).Extension = ".txt") 
                        |> shuffle
                        |> Array.map (FileInfo)
    
    let invertedIndex = Async.RunSynchronously
                            (getWordsByFilesAsync textFilePaths
                            |> assembleInvertedIndexAsync)
    0