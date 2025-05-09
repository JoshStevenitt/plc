{INPUT FILE "S.csv" AS s NOLABELS, 
INPUT FILE "T.csv" AS t NOLABELS}

LET pairs NOLABELS BE
    SELECT 
    COLUMN product.3, COLUMN product.4 
    FROM
    LET product NOLABELS BE 
        PRODUCT s t
    WHERE
    (product.1=product.2) AND
    (product.5=product.6)
    -

LET sortedPairs NOLABELS BE
    SORT pairs ASC
    -

QUERIESEND
OUTPUT sortedPairs TO STANDARD