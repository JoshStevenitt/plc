{INPUT FILE "P.csv" AS p NOLABELS, 
INPUT FILE "Q.csv" AS q NOLABELS, 
INPUT FILE "R.csv" AS r NOLABELS, 
INPUT FILE "S.csv" AS s NOLABELS, 
INPUT FILE "T.csv" AS t NOLABELS}

LET product1 NOLABELS BE
    PRODUCT p q
    -
LET product2 NOLABELS BE
    PRODUCT product1 r
    -
LET product3 NOLABELS BE
    PRODUCT product2 s
    -
LET product4 NOLABELS BE
    PRODUCT product3 t
    -
LET sortedProduct NOLABELS BE
    SORT product4 ASC
    -
QUERIESEND
OUTPUT sortedProduct TO STANDARD