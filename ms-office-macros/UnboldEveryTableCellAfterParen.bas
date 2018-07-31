Sub UnboldEveryTableCellAfterParen()
    Dim Table As Table
    Dim Row As Row
    Dim Cell As Cell

    For Each Table In ActiveDocument.Tables
        For Each Row In Table.Rows
            For Each Cell In Row.Cells
                If SecondCharacter(Cell.Range.Text) = ")" Then
                    With Cell.Range
                        .MoveStart Unit:=wdCharacter, Count:=2
                        .Select
                        .Bold = False
                    End With
                End If
            Next
        Next
    Next
End Sub

Private Function SecondCharacter(Text As String) As String
    SecondCharacter = Mid(Text, 2, 1)
End Function
