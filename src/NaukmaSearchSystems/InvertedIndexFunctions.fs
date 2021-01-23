module NaukmaSearchSystems.InvertedIndexFunctions

    open System.Collections.Generic
    open FSharpx.Control
    open NaukmaSearchSystems.TokenExtractionFunctions
    
    let assembleInvertedIndexAsync (wordsBySources: WordsBySource AsyncSeq) =
        async {
            let entries = Dictionary<string, List<string>>()
            let! wordsBySourcesArray = wordsBySources |> AsyncSeq.toArray
            for currentSourceWithWords in wordsBySourcesArray do
                let otherSources =
                    wordsBySourcesArray |> Array.filter
                        (fun wws -> wws <> currentSourceWithWords)
                for word in currentSourceWithWords.Words do    
                    let sourcesByWord = List<string>()
                    sourcesByWord.Add currentSourceWithWords.Filename                                      
      
                    for otherSource in otherSources do                        
                        let mutable continueWordSearch = otherSource.Words.Count > 0
                        let mutable currentWordNode = otherSource.Words.First
                        while continueWordSearch do
                            match currentWordNode with
                            | currentWord when System.String.Compare(currentWord.Value, word) > 0 ->
                                continueWordSearch <- false 
                            | currentWord when currentWord.Value = word ->                              
                                sourcesByWord.Add otherSource.Filename
                                otherSource.Words.Remove currentWordNode
                                continueWordSearch <- false                   
                            | currentWord when currentWord.Next <> null ->
                                currentWordNode <- currentWordNode.Next
                            | _ ->
                                continueWordSearch <- false   
                    
                    entries.Add(word, sourcesByWord)
                   
            return entries
        }