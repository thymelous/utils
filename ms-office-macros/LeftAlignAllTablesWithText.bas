Sub LeftAlignAllTablesWithText()
    Dim Table As Table

    For Each Table In ActiveDocument.Tables
        Table.LeftPadding = 0
        Table.Rows.LeftIndent = 0
    Next
End Sub
