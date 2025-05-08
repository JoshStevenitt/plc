-- Inputting files
{INPUT FILE "P.csv" AS pp NOLABELS, INPUT FILE "Q.csv" AS qq NOLABELS}
-- Megring
LET merge NOLABELS BE
    MERGE pp qq 
    WITHCONSTRAINT pp.1 = qq.1
    -
LET sortedMerge NOLABELS BE
-- Sorting
    SORT merge ASC
    -
QUERIESEND
--Outputting sortedTable
OUTPUT sortedMerge TO STANDARD