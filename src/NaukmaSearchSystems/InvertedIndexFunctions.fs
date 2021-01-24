module NaukmaSearchSystems.InvertedIndexFunctions

    open System.Collections.Generic
    open System.Runtime.InteropServices
    open System.Runtime.Serialization.Formatters.Binary
    open FSharpx.Control
    open NaukmaSearchSystems.TokenExtractionFunctions
    open NaukmaSearchSystems.UtilityFunctions
    
    type InvertedIndexContext = {
        Documents: string list
        InvertedIndex: Dictionary<string, string list>
    }
    
    type IndexSearchOperand =
    | IndexSearchWord of string
    | IndexSearchOperationResult of string list
    
    let assembleInvertedIndexAsync (wordsBySources: WordsBySource AsyncSeq) =
        async {
            let entries = Dictionary<string, string list>()
            let! wordsBySourcesArray = wordsBySources |> AsyncSeq.toArray

            for currentSourceWithWords in wordsBySourcesArray do
                let otherSources =
                    wordsBySourcesArray |> Array.filter
                        (fun wws -> wws <> currentSourceWithWords)
                for word in currentSourceWithWords.Words do    
                    let mutable sourcesByWord = [currentSourceWithWords.Filename]                                   
          
                    for otherSource in otherSources do     
                        let nodeInOtherSource = findWordInSource word otherSource
                        if nodeInOtherSource <> null && nodeInOtherSource.Value = word then
                           sourcesByWord <- otherSource.Filename :: sourcesByWord
                           otherSource.Words.Remove nodeInOtherSource
                    
                    entries.Add(word, sourcesByWord)

            return {
                Documents = wordsBySourcesArray |> Array.map (fun s -> s.Filename) |> Array.toList
                InvertedIndex = entries
            }
        }
        
    let bIndex_Not (indexContext: InvertedIndexContext) (operand: IndexSearchOperand) =
        let documents =
            match operand with
            | IndexSearchWord word -> indexContext.InvertedIndex.[word]
            | IndexSearchOperationResult result -> result
        IndexSearchOperationResult (List.except documents indexContext.Documents)
           
            
    let bIndex_And (indexContext: InvertedIndexContext) (operand1: IndexSearchOperand) (operand2: IndexSearchOperand) =
        let operand1Documents =
            match operand1 with
            | IndexSearchWord word -> indexContext.InvertedIndex.[word]              
            | IndexSearchOperationResult result -> result
        
        let operand2Documents =
            match operand2 with
            | IndexSearchWord word -> indexContext.InvertedIndex.[word]                
            | IndexSearchOperationResult result -> result

        IndexSearchOperationResult (Set.intersect (Set.ofList operand1Documents) (Set.ofList operand2Documents)
                                    |> Set.toList)
    
    let bIndex_Or (indexContext: InvertedIndexContext) (operand1: IndexSearchOperand) (operand2: IndexSearchOperand) =
        let operand1Documents =
            match operand1 with
            | IndexSearchWord word -> indexContext.InvertedIndex.[word]              
            | IndexSearchOperationResult result -> result
        
        let operand2Documents =
            match operand2 with
            | IndexSearchWord word -> indexContext.InvertedIndex.[word]                
            | IndexSearchOperationResult result -> result
        
        IndexSearchOperationResult (Set.union (Set.ofList operand1Documents) (Set.ofList operand2Documents)
                                    |> Set.toList)