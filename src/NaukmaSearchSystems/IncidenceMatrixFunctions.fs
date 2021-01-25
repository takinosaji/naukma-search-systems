module NaukmaSearchSystems.IncidenceMatrixFunctions

    open System.Collections
    open System.Collections.Generic
    open NaukmaSearchSystems.TokenExtractionFunctions
    open NaukmaSearchSystems.UtilityFunctions

    type SparseMatrixContainer = {
        ColumnIndex: int array
        RowIndex: int array
    }

    type TermDocumentIncidenceMatrix = {
       Words: Dictionary<string, int>
       Documents: string array
       Matrix: SparseMatrixContainer
    }

    type WordBinaryVector = {
        Word: string
        Vector: BitArray
    }

    type MatrixSearchOperation =
    | MatrixSearchWord of string
    | MatrixSearchOperation of BitArray
    
    // https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_row_(CSR,_CRS_or_Yale_format)
    let assembleIncidenceMatrix (wordsBySources: WordsBySource array) =
        let mutable occurenceCount = 0
        let mutable wordsCount = 0
        
        let words = Dictionary<string, int>()
        let columnIndex = List<int>()
        let rowIndex = List<int>()
        
        rowIndex.Add 0

        let wordsBySourcesIndexedArray = wordsBySources |> Array.indexed

        for currentIndexedSourceWithWords in wordsBySourcesIndexedArray do
            let (fileIndex, currentSourceWithWords) = currentIndexedSourceWithWords               
            let otherIndexedSources =
                wordsBySourcesIndexedArray |> Array.filter (fun wws -> fst(wws) > fileIndex)

            let mutable currentNodes = otherIndexedSources |> Array.map (fun wws -> (fst(wws), snd(wws).Words.First))
            for word in currentSourceWithWords.Words do 
                if words.ContainsKey(word) = false then
                    columnIndex.Add fileIndex
                    occurenceCount <- occurenceCount + 1
                           
                    for otherIndexedSource in otherIndexedSources do 
                        let (otherSourceFileIndex, _) = otherIndexedSource
                        let currentNodeIndex = currentNodes |> Array.findIndex (fun n -> fst(n) = otherSourceFileIndex)
                        let nodeInOtherSource = findWordInSource word (currentNodes.[currentNodeIndex] |> snd)

                        if nodeInOtherSource = null then
                           currentNodes.[currentNodeIndex] <- (otherSourceFileIndex, null)
                        else if nodeInOtherSource.Value = word then
                           columnIndex.Add otherSourceFileIndex
                           occurenceCount <- occurenceCount + 1
                           currentNodes.[currentNodeIndex] <- (otherSourceFileIndex, nodeInOtherSource.Next)

                    words.Add(word, wordsCount)
                    wordsCount <- wordsCount + 1
                    rowIndex.Add occurenceCount
        {
            Words = words
            Documents = wordsBySources |> Array.map (fun s -> s.Filename)
            Matrix = {
                ColumnIndex = columnIndex.ToArray()
                RowIndex = rowIndex.ToArray()
            }
        }

    let getBinaryVectorForWord (incidenceMatrix: TermDocumentIncidenceMatrix) (word: string) = 
        let wordIndex =  if incidenceMatrix.Words.ContainsKey(word) then
                            incidenceMatrix.Words.[word]
                         else
                             -1
        let vector = BitArray(incidenceMatrix.Documents.Length)
        let wordBinaryVector = match wordIndex with
                                | -1 -> vector
                                | _ ->  
                                        let rowStart = incidenceMatrix.Matrix.RowIndex.[wordIndex]
                                        let rowEnd = incidenceMatrix.Matrix.RowIndex.[wordIndex + 1]
                                        let columnIndexes = incidenceMatrix.Matrix.ColumnIndex.[rowStart .. rowEnd - 1]
                                        for index in columnIndexes do
                                            vector.[index] <- true
                                        vector
        wordBinaryVector
        
    let bMatrix_Not (matrix: TermDocumentIncidenceMatrix) (operand: MatrixSearchOperation) =
        let vector =
            match operand with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word
            | MatrixSearchOperation operation -> operation
        
        MatrixSearchOperation(vector.Not())
            
    let bMatrix_And (matrix: TermDocumentIncidenceMatrix) (operand1: MatrixSearchOperation) (operand2: MatrixSearchOperation) =
        let operand1Vector =
            match operand1 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperation operation -> operation
        
        let operand2Vector =
            match operand2 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperation operation -> operation
        
        MatrixSearchOperation(operand1Vector.And(operand2Vector))
    
    let bMatrix_Or (matrix: TermDocumentIncidenceMatrix) (operand1: MatrixSearchOperation) (operand2: MatrixSearchOperation) =
        let operand1Vector =
            match operand1 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperation operation -> operation
        
        let operand2Vector =
            match operand2 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperation operation -> operation
        
        MatrixSearchOperation(operand1Vector.Or(operand2Vector))

    let calculateMatrixSearchOperation (matrix: TermDocumentIncidenceMatrix) (operation: MatrixSearchOperation) =
        matrix.Documents
        |> Array.indexed
        |> Array.where (fun (i, _) -> 
            match operation with 
            | MatrixSearchWord word -> (getBinaryVectorForWord matrix word).[i]  
            | MatrixSearchOperation result -> result.[i])
        |> Array.map (fun (_, fileName) -> fileName)