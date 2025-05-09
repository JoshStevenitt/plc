{INPUT FILE "F.csv" AS f NOLABELS, 
INPUT FILE "G.csv" AS g NOLABELS}

LET composition NOLABELS BE
    SELECT 
    COLUMN product.1, COLUMN product.6 
    FROM
    LET product NOLABELS BE 
        PRODUCT f g
    WHERE 
    (NOT (product.2 = '')) AND 
    (NOT (product.3='')) AND
    (product.2=product.4) AND
    (product.3=product.5)
    -

LET sortedComposition NOLABELS BE
    SORT composition ASC
    -

QUERIESEND
OUTPUT sortedComposition TO STANDARD