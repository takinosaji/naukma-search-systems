module NaukmaSearchSystems.InvertedIndexFunctions

    open System.Collections.Generic
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
    
    let assembleInvertedIndex (wordsBySources: WordsBySource array) =
        let entries = Dictionary<string, string list>()

        let wordsBySourcesIndexedArray = wordsBySources |> Array.indexed
                   
        for currentIndexedSourceWithWords in wordsBySourcesIndexedArray do
            let (fileIndex, currentSourceWithWords) = currentIndexedSourceWithWords               
            let otherIndexedSources =
                wordsBySourcesIndexedArray |> Array.filter (fun wws -> fst(wws) > fileIndex)

            let mutable currentNodes = otherIndexedSources |> Array.map (fun wws -> (fst(wws), snd(wws).Words.First))
            for word in currentSourceWithWords.Words do 
               if entries.ContainsKey(word) = false then   
                  let mutable sourcesByWord = [currentSourceWithWords.Filename]                                   
      
                  for otherIndexedSource in otherIndexedSources do 
                     let (otherSourceFileIndex, otherSource) = otherIndexedSource
                     let currentNodeIndex = currentNodes |> Array.findIndex (fun n -> fst(n) = otherSourceFileIndex)
                     let nodeInOtherSource = findWordInSource word (currentNodes.[currentNodeIndex] |> snd)  

                     if nodeInOtherSource = null then
                         currentNodes.[currentNodeIndex] <- (otherSourceFileIndex, null)
                     else if nodeInOtherSource.Value = word then
                         sourcesByWord <- otherSource.Filename :: sourcesByWord
                         currentNodes.[currentNodeIndex] <- (otherSourceFileIndex, nodeInOtherSource.Next)

                  entries.Add(word, sourcesByWord)
        {
            Documents = wordsBySources |> Array.map (fun s -> s.Filename) |> Array.toList
            InvertedIndex = entries
        }
    
    let getFilesListFromIndex (indexContext: InvertedIndexContext) (word: string) =
           if indexContext.InvertedIndex.ContainsKey(word) then
               indexContext.InvertedIndex.[word] 
           else
               list.Empty

    let bIndex_Not (indexContext: InvertedIndexContext) (operand: IndexSearchOperand) =
        let documents =
            match operand with
            | IndexSearchWord word -> getFilesListFromIndex indexContext word
            | IndexSearchOperationResult result -> result
        IndexSearchOperationResult (List.except documents indexContext.Documents)
           
            
    let bIndex_And (indexContext: InvertedIndexContext) (operand1: IndexSearchOperand) (operand2: IndexSearchOperand) =
        let operand1Documents =
            match operand1 with
            | IndexSearchWord word -> getFilesListFromIndex indexContext word              
            | IndexSearchOperationResult result -> result
        
        let operand2Documents =
            match operand2 with
            | IndexSearchWord word -> getFilesListFromIndex indexContext word                
            | IndexSearchOperationResult result -> result

        IndexSearchOperationResult (Set.intersect (Set.ofList operand1Documents) (Set.ofList operand2Documents)
                                    |> Set.toList)
    
    let bIndex_Or (indexContext: InvertedIndexContext) (operand1: IndexSearchOperand) (operand2: IndexSearchOperand) =
        let operand1Documents =
            match operand1 with
            | IndexSearchWord word -> getFilesListFromIndex indexContext word              
            | IndexSearchOperationResult result -> result
        
        let operand2Documents =
            match operand2 with
            | IndexSearchWord word -> getFilesListFromIndex indexContext word                
            | IndexSearchOperationResult result -> result
        
        IndexSearchOperationResult (Set.union (Set.ofList operand1Documents) (Set.ofList operand2Documents)
                                    |> Set.toList)

   