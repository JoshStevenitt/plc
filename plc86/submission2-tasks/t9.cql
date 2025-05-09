{INPUT FILE "R.csv" AS r NOLABELS}
LET pathsLength2 NOLABELS BE
    SELECT COLUMN product.1, COLUMN product.4
    FROM
    LET product NOLABELS BE
        PRODUCT r r
    WHERE product.2=product.3
    -

LET pathsLength3 NOLABELS BE
    SELECT COLUMN product.1, COLUMN product.4
    FROM
    LET product NOLABELS BE
        PRODUCT pathsLength2 r
    WHERE product.2=product.3
    -

LET sortedPaths NOLABELS BE
    SORT pathsLength3 ASC
    -
QUERIESEND
OUTPUT sortedPaths TO STANDARD

