module NaukmaSearchSystems.IncidenceMatrixFunctions

    open System.Collections
    open System.Collections.Generic
    open FSharpx.Control
    open NaukmaSearchSystems.TokenExtractionFunctions
    open NaukmaSearchSystems.UtilityFunctions

    type SparseMatrixContainer = {
        ColumnIndex: int array
        RowIndex: int array
    }

    type TermDocumentIncidenceMatrix = {
       Words: List<string>
       Documents: string array
       Matrix: SparseMatrixContainer
    }

    type WordBinaryVector = {
        Word: string
        Vector: BitArray
    }

    type MatrixSearchOperand =
    | MatrixSearchWord of string
    | MatrixSearchOperationResult of BitArray
    
    // Author: Yulia Halytsia
    // https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_row_(CSR,_CRS_or_Yale_format)
    let assembleIncidenceMatrixAsync (wordsBySources: WordsBySource AsyncSeq) =
        async {
            let mutable occurenceCount = 0
            
            let words = List<string>()
            let columnIndex = List<int>()
            let rowIndex = List<int>()
            
            rowIndex.Add 0

            let! wordsBySourcesArray = wordsBySources |> AsyncSeq.toArray
            let wordsBySourcesIndexedArray = wordsBySourcesArray |> Array.indexed
            
            for currentIndexedSourceWithWords in wordsBySourcesIndexedArray do
                let (fileIndex, currentSourceWithWords) = currentIndexedSourceWithWords               
                let otherIndexedSources =
                    wordsBySourcesIndexedArray |> Array.filter
                        (fun wws -> wws <> currentIndexedSourceWithWords)
                        
                for word in currentSourceWithWords.Words do    
                    columnIndex.Add fileIndex
                    occurenceCount <- occurenceCount + 1      
                               
                    for otherIndexedSource in otherIndexedSources do 
                        let (otherSourceFileIndex, otherSource) = otherIndexedSource
                        let nodeInOtherSource = findWordInSource word otherSource
                        if nodeInOtherSource <> null && nodeInOtherSource.Value = word then
                            columnIndex.Add otherSourceFileIndex
                            occurenceCount <- occurenceCount + 1
                            otherSource.Words.Remove nodeInOtherSource

                    words.Add word
                    rowIndex.Add occurenceCount
            let matrix = {
                   ColumnIndex = columnIndex.ToArray()
                   RowIndex = rowIndex.ToArray()
                 }
            return {
                Words = words
                Documents = wordsBySourcesArray |> Array.map (fun s -> s.Filename)
                Matrix = matrix
            }
        }

    let getBinaryVectorForWord (incidenceMatrix: TermDocumentIncidenceMatrix) (word: string) = 
        let wordIndex = incidenceMatrix.Words.IndexOf(word) // improve efficiency
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
        
    let bMatrix_Not (matrix: TermDocumentIncidenceMatrix) (operand: MatrixSearchOperand) =
        let vector =
            match operand with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word
            | MatrixSearchOperationResult result -> result
        
        MatrixSearchOperationResult(vector.Not())
            
    let bMatrix_And (matrix: TermDocumentIncidenceMatrix) (operand1: MatrixSearchOperand) (operand2: MatrixSearchOperand) =
        let operand1Vector =
            match operand1 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperationResult result -> result
        
        let operand2Vector =
            match operand2 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperationResult result -> result
        
        MatrixSearchOperationResult(operand1Vector.And(operand2Vector))
    
    let bMatrix_Or (matrix: TermDocumentIncidenceMatrix) (operand1: MatrixSearchOperand) (operand2: MatrixSearchOperand) =
        let operand1Vector =
            match operand1 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperationResult result -> result
        
        let operand2Vector =
            match operand2 with
            | MatrixSearchWord word -> getBinaryVectorForWord matrix word                
            | MatrixSearchOperationResult result -> result
        
        MatrixSearchOperationResult(operand1Vector.Or(operand2Vector))
//        let vector = operand1Vector.Or(operand2Vector)
//        matrix.Documents
//        |> Array.indexed
//        |> Array.where (fun (i, _) -> vector.[i])
