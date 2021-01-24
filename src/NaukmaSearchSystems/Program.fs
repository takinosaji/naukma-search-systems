open System.IO
open NaukmaSearchSystems
open FSharp.Control

open TokenExtractionFunctions
open InvertedIndexFunctions
open IncidenceMatrixFunctions
open OutputFunctions

[<EntryPoint>]
let main _ =        
    let textFilePaths = Directory.GetFiles($"{Directory.GetCurrentDirectory()}\..\..\..\..\..\data")
                        |> Array.filter (fun filePath -> FileInfo(filePath).Extension = ".txt") 
                        |> shuffle
                        |> Array.map (FileInfo)
    
    let invertedIndexContext = Async.RunSynchronously
                                (getWordsByFilesAsync textFilePaths
                                |> assembleInvertedIndexAsync)
    let incidenceMatrix = Async.RunSynchronously
                            (getWordsByFilesAsync textFilePaths
                            |> assembleIncidenceMatrixAsync)
         
    let indexSize = (serializeAsByteArray invertedIndexContext.InvertedIndex).Length
    let matrixSize = (serializeAsByteArray incidenceMatrix.Matrix).Length
         
    let indexNotOp = bIndex_Not invertedIndexContext
    let indexAndOp = bIndex_And invertedIndexContext
    let indexOrOp = bIndex_Or invertedIndexContext
    
    let matrixNotOp = bMatrix_Not incidenceMatrix
    let matrixAndOp = bMatrix_And incidenceMatrix
    let matrixOrOp = bMatrix_Or incidenceMatrix
    
    let indexSearchResult1 = indexAndOp (IndexSearchWord "man") (IndexSearchWord "action")
    let indexSearchResult2 = indexOrOp (IndexSearchWord "woman") (indexNotOp (IndexSearchWord "action"))
    
    let matrixSearchResult1 = matrixAndOp (MatrixSearchWord "man") (MatrixSearchWord "action")
    let matrixSearchResult2 = matrixOrOp (MatrixSearchWord "woman") (matrixNotOp (MatrixSearchWord "action"))
    0