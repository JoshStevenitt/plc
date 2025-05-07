{INPUT FILE "A.csv" AS aa NOLABELS}
LET existenceCheck NOLABELS BE
    SELECT COLUMNS *
    FROM aa
    WHERE NOT (aa.2 = '') 
    -
LET sortedExistenceCheck NOLABELS BE
    SORT existenceCheck ASC
    -
QUERIESEND
OUTPUT sortedExistenceCheck TO STANDARD