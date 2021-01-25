module NaukmaSearchSystems.UtilityFunctions

    open System.Collections.Generic

    let findWordInSource (word: string) (wordNode: LinkedListNode<string>) = 
            let mutable continueWordSearch = wordNode <> null
            let mutable currentWordNode = wordNode
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