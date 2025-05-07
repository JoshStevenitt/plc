{ INPUT FILE "A.csv" AS aa NOLABELS, INPUT FILE "B.csv" AS bb NOLABELS } 
LET crossProduct NOLABELS BE 
    PRODUCT aa bb 
    -
LET sortedCrossProduct NOLABELS BE
    SORT crossProduct ASC
    -
QUERIESEND
OUTPUT sortedCrossProduct TO STANDARD