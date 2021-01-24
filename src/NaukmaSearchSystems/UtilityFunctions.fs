module NaukmaSearchSystems.UtilityFunctions

    open NaukmaSearchSystems.TokenExtractionFunctions

    let findWordInSource (word: string) (source: WordsBySource) = 
            let mutable continueWordSearch = source.Words.Count > 0
            let mutable currentWordNode = source.Words.First
            while continueWordSearch do
                match currentWordNode with
                | currentWord when System.String.Compare(currentWord.Value, word) > 0 ->
                    continueWordSearch <- false 
                | currentWord when currentWord.Value = word ->  
                    continueWordSearch <- false
                | currentWord when currentWord.Next <> null ->
                    currentWordNode <- currentWordNode.Next
                | _ ->
                    continueWordSearch <- false
            currentWordNode