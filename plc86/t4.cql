{INPUT FILE "A.csv" AS aa NOLABELS}
LET newColumn NOLABELS BE
    ADDBLANKTO aa COLUMN 2
    -
LET fooColumn NOLABELS BE
    FILL 'foo' newColumn COLUMN 2
    -
LET copyingAndConstants NOLABELS BE
    SELECT COLUMN fooColumn.1, COLUMN fooColumn.2, COLUMN fooColumn.1
    FROM foo
    -
LET sortedCopyingAndConstants NOLABELS BE
    SORT copyingAndConstants ASC
    -
QUERIESEND
OUTPUT sortedCopyingAndConstants TO STANDARD