{INPUT FILE "A.csv" AS aa NOLABELS}
LET permutationDropMatch NOLABELS BE
    SELECT COLUMN aa.3, COLUMN aa.1
    FROM aa
    WHERE aa.1 = aa.2 
    -
LET sortedPermutationDropMatch NOLABELS BE
    SORT permutationDropMatch ASC
    -
QUERIESEND
OUTPUT sortedPermutationDropMatch TO STANDARD
