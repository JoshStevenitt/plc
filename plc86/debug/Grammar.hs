{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
module Grammar where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31
        = HappyTerminal (PosnToken)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn4 t4
        | HappyAbsSyn5 t5
        | HappyAbsSyn6 t6
        | HappyAbsSyn7 t7
        | HappyAbsSyn8 t8
        | HappyAbsSyn9 t9
        | HappyAbsSyn10 t10
        | HappyAbsSyn11 t11
        | HappyAbsSyn12 t12
        | HappyAbsSyn13 t13
        | HappyAbsSyn14 t14
        | HappyAbsSyn15 t15
        | HappyAbsSyn16 t16
        | HappyAbsSyn17 t17
        | HappyAbsSyn18 t18
        | HappyAbsSyn19 t19
        | HappyAbsSyn20 t20
        | HappyAbsSyn21 t21
        | HappyAbsSyn22 t22
        | HappyAbsSyn23 t23
        | HappyAbsSyn24 t24
        | HappyAbsSyn25 t25
        | HappyAbsSyn26 t26
        | HappyAbsSyn27 t27
        | HappyAbsSyn28 t28
        | HappyAbsSyn29 t29
        | HappyAbsSyn30 t30
        | HappyAbsSyn31 t31

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x7e\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x10\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x10\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x9f\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x10\x30\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x01\x03\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x10\x30\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x01\x03\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x10\x30\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x01\x03\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x10\x30\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x81\x07\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x10\x78\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x10\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x81\x07\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x7e\x00\x00\x00\x02\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x09\x02\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x10\x78\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Start","Inputs","Input","TableAssignment","TableName","ColumnLabels","ColumnLabels2","ColumnReference","Output","OutputType","Position","Axis","Queries","TableExpression","Query","Selection","SortClause","MaxClause","Distinct","ColumnChoice","WhereClause","PlusClause","JoinClause","JoinOperator","BooleanExpression","SubExpression","IndexExpression","Number","INPUT","OUTPUT","LET","BE","QUERIESEND","MERGE","SELECT","TO","STANDARD","FILE","PRODUCT","SORT","INSERT","FILL","DELETE","CLEAR","COLUMN","COLUMNS","ROW","FROMTABLES","FROM","JOIN","INNER","LEFT","RIGHT","FULL","OUTER","TABLE","AS","WITHLABELS","NOLABELS","WHERE","WITHCONSTRAINT","AND","OR","NOT","INDEX","MAX","PLUS","DISTINCT","ASC","DESC","ADDBLANKTO","'='","'+'","'*'","'/'","\"//\"","'%'","'^'","'{'","'}'","'-'","','","'['","']'","'.'","'('","')'","int","alphaLower","alphaUpper","digits","Filename","alphaNumericString","positiveNum","negativeNum","var","%eof"]
        bit_start = st               Prelude.* 100
        bit_end   = (st Prelude.+ 1) Prelude.* 100
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..99]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xd0\xff\xff\xff\xd0\xff\xff\xff\x12\x00\x00\x00\xe7\xff\xff\xff\x0d\x00\x00\x00\x14\x00\x00\x00\x80\x00\x00\x00\x15\x00\x00\x00\x58\x00\x00\x00\x5e\x00\x00\x00\x9b\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x82\x00\x00\x00\xb8\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\x00\x00\x00\xbf\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x94\x00\x00\x00\x00\x00\x00\x00\x89\x00\x00\x00\xac\x00\x00\x00\x8a\x00\x00\x00\x8a\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8e\x00\x00\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x93\x00\x00\x00\x96\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\x00\x00\x00\x9f\x00\x00\x00\x97\x00\x00\x00\x88\x00\x00\x00\x90\x00\x00\x00\x88\x00\x00\x00\x98\x00\x00\x00\x98\x00\x00\x00\x13\x00\x00\x00\x98\x00\x00\x00\xae\x00\x00\x00\x5f\x00\x00\x00\x99\x00\x00\x00\x61\x00\x00\x00\x00\x00\x00\x00\xad\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x00\x00\x8b\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\xa4\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\x00\x00\x00\xb2\x00\x00\x00\xb3\x00\x00\x00\xe2\xff\xff\xff\xb5\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xb6\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xff\xff\x00\x00\x00\x00\xe2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbb\x00\x00\x00\xb7\x00\x00\x00\xff\xff\xff\xff\x92\x00\x00\x00\xf8\xff\xff\xff\x56\x00\x00\x00\xfc\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xed\xff\xff\xff\xe2\xff\xff\xff\xe2\xff\xff\xff\xe2\xff\xff\xff\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xff\xff\xfc\xff\xff\xff\x83\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\xed\xff\xff\xff\x65\x00\x00\x00\x27\x00\x00\x00\x27\x00\x00\x00\x27\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x00\x00\x00\xd8\x00\x00\x00\xbc\x00\x00\x00\xe2\xff\xff\xff\xfc\xff\xff\xff\x00\x00\x00\x00\xd7\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\x56\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xc9\x00\x00\x00\xce\x00\x00\x00\xd0\x00\x00\x00\xd1\x00\x00\x00\xd2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x00\x00\x00\xe2\xff\xff\xff\xfc\xff\xff\xff\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xeb\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\xe0\x00\x00\x00\xe7\x00\x00\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\x00\x00\x00\x00\x00\x00\x00\xed\x00\x00\x00\x00\x00\x00\x00\xa2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x00\x00\x00\xe5\x00\x00\x00\xf3\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x00\x00\x00\xf7\x00\x00\x00\xf8\x00\x00\x00\x00\x00\x00\x00\xf9\x00\x00\x00\xf4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\x00\x00\x00\xfc\x00\x00\x00\xfd\x00\x00\x00\xfe\x00\x00\x00\xff\x00\x00\x00\xee\x00\x00\x00\x00\x01\x00\x00\xef\x00\x00\x00\xf1\x00\x00\x00\x03\x01\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x01\x00\x00\x06\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x00\x00\xaf\x00\x00\x00\x1e\x00\x00\x00\x07\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x00\x00\x00\x9c\x00\x00\x00\x9e\x00\x00\x00\xa0\x00\x00\x00\xa3\x00\x00\x00\xa5\x00\x00\x00\xa7\x00\x00\x00\x37\x00\x00\x00\x4f\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xff\xff\xfd\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\xff\xff\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\xff\xff\xff\x00\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\xe4\xff\xff\xff\x00\x00\x00\x00\xd8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xff\xff\xf1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\xff\xff\xff\x00\x00\x00\x00\xd9\xff\xff\xff\xb5\xff\xff\xff\xb4\xff\xff\xff\x00\x00\x00\x00\xd7\xff\xff\xff\xe3\xff\xff\xff\xe2\xff\xff\xff\xdb\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xff\xff\x00\x00\x00\x00\xdd\xff\xff\xff\x00\x00\x00\x00\xf7\xff\xff\xff\xf0\xff\xff\xff\xf6\xff\xff\xff\x00\x00\x00\x00\xec\xff\xff\xff\xee\xff\xff\xff\xed\xff\xff\xff\xe0\xff\xff\xff\xe1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xff\xff\xe5\xff\xff\xff\x00\x00\x00\x00\xc0\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\xbf\xff\xff\xff\x00\x00\x00\x00\xc1\xff\xff\xff\xd5\xff\xff\xff\xd4\xff\xff\xff\x00\x00\x00\x00\xef\xff\xff\xff\xe9\xff\xff\xff\xd1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\xff\xff\xc0\xff\xff\xff\xc5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\xff\xff\xf4\xff\xff\xff\xc6\xff\xff\xff\xc7\xff\xff\xff\xc3\xff\xff\xff\xc4\xff\xff\xff\xbb\xff\xff\xff\x00\x00\x00\x00\xb6\xff\xff\xff\xb7\xff\xff\xff\xb8\xff\xff\xff\xb9\xff\xff\xff\xba\xff\xff\xff\xbc\xff\xff\xff\xbd\xff\xff\xff\xc8\xff\xff\xff\xd3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\xff\xff\x00\x00\x00\x00\xd2\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xff\xff\xe7\xff\xff\xff\xd0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff\xff\xff\xca\xff\xff\xff\xcb\xff\xff\xff\xcc\xff\xff\xff\xcd\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xce\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x03\x00\x00\x00\x03\x00\x00\x00\x33\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x23\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2c\x00\x00\x00\x25\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x3a\x00\x00\x00\x04\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x44\x00\x00\x00\x3a\x00\x00\x00\x2c\x00\x00\x00\x2b\x00\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x45\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x3b\x00\x00\x00\x2b\x00\x00\x00\x07\x00\x00\x00\x0d\x00\x00\x00\x04\x00\x00\x00\x3a\x00\x00\x00\x3a\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x07\x00\x00\x00\x41\x00\x00\x00\x41\x00\x00\x00\x34\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x36\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x04\x00\x00\x00\x2d\x00\x00\x00\x40\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x3c\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x35\x00\x00\x00\x41\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x3c\x00\x00\x00\x03\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x03\x00\x00\x00\x41\x00\x00\x00\x05\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x04\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x16\x00\x00\x00\x0a\x00\x00\x00\x35\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x3b\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x1d\x00\x00\x00\x11\x00\x00\x00\x35\x00\x00\x00\x13\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x41\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x04\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x41\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x41\x00\x00\x00\x08\x00\x00\x00\x37\x00\x00\x00\x35\x00\x00\x00\x41\x00\x00\x00\x41\x00\x00\x00\x3c\x00\x00\x00\x36\x00\x00\x00\x21\x00\x00\x00\x41\x00\x00\x00\x44\x00\x00\x00\x44\x00\x00\x00\x26\x00\x00\x00\x36\x00\x00\x00\x41\x00\x00\x00\x40\x00\x00\x00\x28\x00\x00\x00\x38\x00\x00\x00\x15\x00\x00\x00\x41\x00\x00\x00\x41\x00\x00\x00\x20\x00\x00\x00\x04\x00\x00\x00\x3c\x00\x00\x00\x07\x00\x00\x00\x16\x00\x00\x00\x3c\x00\x00\x00\x2e\x00\x00\x00\x2c\x00\x00\x00\x27\x00\x00\x00\x16\x00\x00\x00\x2c\x00\x00\x00\x16\x00\x00\x00\x16\x00\x00\x00\x16\x00\x00\x00\x21\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x3c\x00\x00\x00\x39\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x41\x00\x00\x00\x05\x00\x00\x00\x41\x00\x00\x00\x04\x00\x00\x00\x3b\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x41\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x10\x00\x00\x00\x06\x00\x00\x00\x17\x00\x00\x00\x12\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x0b\x00\x00\x00\x0a\x00\x00\x00\x04\x00\x00\x00\x0b\x00\x00\x00\xff\xff\xff\xff\x06\x00\x00\x00\xff\xff\xff\xff\x1b\x00\x00\x00\x0c\x00\x00\x00\x13\x00\x00\x00\x0a\x00\x00\x00\xff\xff\xff\xff\x0b\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x00\x00\xff\xff\xff\xff\x0f\x00\x00\x00\xff\xff\xff\xff\x13\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x6a\x00\x00\x00\x6a\x00\x00\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x79\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\xc3\xff\xff\xff\x61\x00\x00\x00\x07\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x78\x00\x00\x00\x79\x00\x00\x00\x62\x00\x00\x00\x59\x00\x00\x00\x78\x00\x00\x00\x79\x00\x00\x00\x64\x00\x00\x00\x67\x00\x00\x00\x67\x00\x00\x00\x13\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x63\x00\x00\x00\x82\x00\x00\x00\xc3\xff\xff\xff\x25\x00\x00\x00\x68\x00\x00\x00\x8b\x00\x00\x00\xff\xff\xff\xff\x67\x00\x00\x00\x59\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x5a\x00\x00\x00\x59\x00\x00\x00\x8a\x00\x00\x00\x25\x00\x00\x00\x5a\x00\x00\x00\xa4\x00\x00\x00\x59\x00\x00\x00\x6b\x00\x00\x00\x6b\x00\x00\x00\x5a\x00\x00\x00\x59\x00\x00\x00\x42\x00\x00\x00\x43\x00\x00\x00\x5a\x00\x00\x00\x13\x00\x00\x00\x13\x00\x00\x00\x0a\x00\x00\x00\x5b\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x6e\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x09\x00\x00\x00\x6c\x00\x00\x00\x5c\x00\x00\x00\x6d\x00\x00\x00\x5e\x00\x00\x00\x7e\x00\x00\x00\x7f\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x59\x00\x00\x00\x70\x00\x00\x00\x0f\x00\x00\x00\x5a\x00\x00\x00\x59\x00\x00\x00\x52\x00\x00\x00\x07\x00\x00\x00\x5a\x00\x00\x00\x59\x00\x00\x00\x76\x00\x00\x00\x53\x00\x00\x00\x5a\x00\x00\x00\x59\x00\x00\x00\x7b\x00\x00\x00\x0c\x00\x00\x00\x5a\x00\x00\x00\x0d\x00\x00\x00\x0c\x00\x00\x00\x7c\x00\x00\x00\x0d\x00\x00\x00\x7d\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x7c\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x8f\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\xa6\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x67\x00\x00\x00\x70\x00\x00\x00\x71\x00\x00\x00\x72\x00\x00\x00\x73\x00\x00\x00\x74\x00\x00\x00\x75\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x95\x00\x00\x00\x96\x00\x00\x00\x1b\x00\x00\x00\x70\x00\x00\x00\x71\x00\x00\x00\x72\x00\x00\x00\x73\x00\x00\x00\x74\x00\x00\x00\x75\x00\x00\x00\x97\x00\x00\x00\x08\x00\x00\x00\x76\x00\x00\x00\x9b\x00\x00\x00\x9c\x00\x00\x00\x9d\x00\x00\x00\x9e\x00\x00\x00\x9f\x00\x00\x00\x89\x00\x00\x00\x70\x00\x00\x00\x71\x00\x00\x00\x72\x00\x00\x00\x73\x00\x00\x00\x74\x00\x00\x00\x75\x00\x00\x00\x10\x00\x00\x00\x47\x00\x00\x00\x76\x00\x00\x00\x48\x00\x00\x00\x47\x00\x00\x00\x15\x00\x00\x00\x48\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x57\x00\x00\x00\x58\x00\x00\x00\x78\x00\x00\x00\x79\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x05\x00\x00\x00\x13\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x19\x00\x00\x00\x11\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x8c\x00\x00\x00\x11\x00\x00\x00\x87\x00\x00\x00\x5e\x00\x00\x00\x86\x00\x00\x00\x5e\x00\x00\x00\x85\x00\x00\x00\x5e\x00\x00\x00\x84\x00\x00\x00\x5e\x00\x00\x00\x19\x00\x00\x00\x83\x00\x00\x00\x5e\x00\x00\x00\x82\x00\x00\x00\x5e\x00\x00\x00\x80\x00\x00\x00\x5e\x00\x00\x00\x13\x00\x00\x00\x94\x00\x00\x00\x5e\x00\x00\x00\x13\x00\x00\x00\x28\x00\x00\x00\x27\x00\x00\x00\x38\x00\x00\x00\x13\x00\x00\x00\x13\x00\x00\x00\x4a\x00\x00\x00\x4c\x00\x00\x00\x59\x00\x00\x00\x13\x00\x00\x00\x32\x00\x00\x00\x31\x00\x00\x00\x36\x00\x00\x00\x50\x00\x00\x00\x2d\x00\x00\x00\x4e\x00\x00\x00\x3f\x00\x00\x00\x4d\x00\x00\x00\x66\x00\x00\x00\x13\x00\x00\x00\x13\x00\x00\x00\x8f\x00\x00\x00\x93\x00\x00\x00\x4a\x00\x00\x00\x1e\x00\x00\x00\xa4\x00\x00\x00\x51\x00\x00\x00\x64\x00\x00\x00\x77\x00\x00\x00\x92\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\xa2\x00\x00\x00\xa1\x00\x00\x00\xa0\x00\x00\x00\xa6\x00\x00\x00\x6c\x00\x00\x00\x03\x00\x00\x00\x0a\x00\x00\x00\x67\x00\x00\x00\x7a\x00\x00\x00\x13\x00\x00\x00\x15\x00\x00\x00\x2d\x00\x00\x00\x25\x00\x00\x00\x13\x00\x00\x00\x36\x00\x00\x00\x94\x00\x00\x00\x34\x00\x00\x00\x33\x00\x00\x00\x13\x00\x00\x00\x32\x00\x00\x00\x2f\x00\x00\x00\x2e\x00\x00\x00\x2d\x00\x00\x00\x28\x00\x00\x00\x40\x00\x00\x00\x2b\x00\x00\x00\x99\x00\x00\x00\x3d\x00\x00\x00\x44\x00\x00\x00\x43\x00\x00\x00\x3f\x00\x00\x00\x4a\x00\x00\x00\x48\x00\x00\x00\x39\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x4e\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x38\x00\x00\x00\x55\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 75) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68),
        (69 , happyReduce_69),
        (70 , happyReduce_70),
        (71 , happyReduce_71),
        (72 , happyReduce_72),
        (73 , happyReduce_73),
        (74 , happyReduce_74),
        (75 , happyReduce_75)
        ]

happy_n_terms = 70 :: Prelude.Int
happy_n_nonterms = 28 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happyReduce 5# 0# happyReduction_1
happyReduction_1 ((HappyAbsSyn12  happy_var_5) `HappyStk`
        (HappyAbsSyn16  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn4
                 (Start happy_var_2 happy_var_4 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn5
                 (InputsCons happy_var_1 happy_var_3
        )
happyReduction_2 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_1  1# happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn5
                 (InputSingle happy_var_1
        )
happyReduction_3 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happyReduce 5# 2# happyReduction_4
happyReduction_4 ((HappyAbsSyn7  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (PT _ (TokenFilename happy_var_3))) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (Input happy_var_3 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_2  3# happyReduction_5
happyReduction_5 _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (NoLabels happy_var_1
        )
happyReduction_5 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happySpecReduce_3  3# happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (WithLabels happy_var_1 happy_var_3
        )
happyReduction_6 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happySpecReduce_1  4# happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (TokenAlphaNumericString happy_var_1)))
         =  HappyAbsSyn8
                 (TableRef happy_var_1
        )
happyReduction_7 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_3  5# happyReduction_8
happyReduction_8 _
        (HappyAbsSyn10  happy_var_2)
        _
         =  HappyAbsSyn9
                 (LabelConstructor happy_var_2
        )
happyReduction_8 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  6# happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
        _
        (HappyTerminal (PT _ (TokenAlphaNumericString happy_var_1)))
         =  HappyAbsSyn10
                 (LabelsMultiple happy_var_1 happy_var_3
        )
happyReduction_9 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_1  6# happyReduction_10
happyReduction_10 (HappyTerminal (PT _ (TokenAlphaNumericString happy_var_1)))
         =  HappyAbsSyn10
                 (LabelSingular happy_var_1
        )
happyReduction_10 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 (HappyTerminal (PT _ (TokenAlphaNumericString happy_var_3)))
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn11
                 (AlphaColumn happy_var_1 happy_var_3
        )
happyReduction_11 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_3  7# happyReduction_12
happyReduction_12 (HappyTerminal (PT _ (TokenInt happy_var_3)))
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn11
                 (IntegerColumn happy_var_1 happy_var_3
        )
happyReduction_12 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happyReduce 4# 8# happyReduction_13
happyReduction_13 ((HappyAbsSyn13  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn8  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (OutputConstruct happy_var_2 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_1  9# happyReduction_14
happyReduction_14 _
         =  HappyAbsSyn13
                 (Standard
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_2  9# happyReduction_15
happyReduction_15 (HappyTerminal (PT _ (TokenFilename happy_var_2)))
        _
         =  HappyAbsSyn13
                 (File happy_var_2
        )
happyReduction_15 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_3  10# happyReduction_16
happyReduction_16 (HappyTerminal (PT _ (TokenInt happy_var_3)))
        _
        (HappyTerminal (PT _ (TokenInt happy_var_1)))
         =  HappyAbsSyn14
                 (Comma happy_var_1 happy_var_3
        )
happyReduction_16 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_2  11# happyReduction_17
happyReduction_17 (HappyTerminal (PT _ (TokenInt happy_var_2)))
        _
         =  HappyAbsSyn15
                 (ColumnInt happy_var_2
        )
happyReduction_17 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_2  11# happyReduction_18
happyReduction_18 (HappyTerminal (PT _ (TokenAlphaNumericString happy_var_2)))
        _
         =  HappyAbsSyn15
                 (ColumnAlpha happy_var_2
        )
happyReduction_18 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_2  11# happyReduction_19
happyReduction_19 (HappyTerminal (PT _ (TokenInt happy_var_2)))
        _
         =  HappyAbsSyn15
                 (Row happy_var_2
        )
happyReduction_19 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happyReduce 6# 12# happyReduction_20
happyReduction_20 ((HappyAbsSyn16  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn7  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (QueryLet happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_1  12# happyReduction_21
happyReduction_21 _
         =  HappyAbsSyn16
                 (QueryEnd
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_1  13# happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn17
                 (SingleTable happy_var_1
        )
happyReduction_22 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happyReduce 4# 13# happyReduction_23
happyReduction_23 ((HappyAbsSyn18  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn7  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn17
                 (TableLetQuery happy_var_2 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happyReduce 4# 13# happyReduction_24
happyReduction_24 ((HappyAbsSyn26  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn7  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn17
                 (TableLetJoin happy_var_2 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_3  13# happyReduction_25
happyReduction_25 _
        (HappyAbsSyn17  happy_var_2)
        _
         =  HappyAbsSyn17
                 (SingleTableExpression happy_var_2
        )
happyReduction_25 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happyReduce 5# 14# happyReduction_26
happyReduction_26 ((HappyAbsSyn28  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn8  happy_var_3) `HappyStk`
        (HappyAbsSyn8  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (Merge happy_var_2 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_1  14# happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn18
                 (SelecterQuery happy_var_1
        )
happyReduction_27 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happySpecReduce_3  14# happyReduction_28
happyReduction_28 (HappyAbsSyn8  happy_var_3)
        (HappyAbsSyn8  happy_var_2)
        _
         =  HappyAbsSyn18
                 (Product happy_var_2 happy_var_3
        )
happyReduction_28 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_3  14# happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_3)
        (HappyAbsSyn8  happy_var_2)
        _
         =  HappyAbsSyn18
                 (Sort happy_var_2 happy_var_3
        )
happyReduction_29 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happyReduce 4# 14# happyReduction_30
happyReduction_30 ((HappyAbsSyn14  happy_var_4) `HappyStk`
        (HappyAbsSyn8  happy_var_3) `HappyStk`
        (HappyTerminal (PT _ (TokenString happy_var_2))) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (Insert happy_var_2 happy_var_3 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happyReduce 4# 14# happyReduction_31
happyReduction_31 ((HappyAbsSyn15  happy_var_4) `HappyStk`
        (HappyAbsSyn8  happy_var_3) `HappyStk`
        (HappyTerminal (PT _ (TokenString happy_var_2))) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (Fill happy_var_2 happy_var_3 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_3  14# happyReduction_32
happyReduction_32 (HappyAbsSyn15  happy_var_3)
        (HappyAbsSyn8  happy_var_2)
        _
         =  HappyAbsSyn18
                 (Delete happy_var_2 happy_var_3
        )
happyReduction_32 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_3  14# happyReduction_33
happyReduction_33 (HappyAbsSyn14  happy_var_3)
        (HappyAbsSyn8  happy_var_2)
        _
         =  HappyAbsSyn18
                 (Clear happy_var_2 happy_var_3
        )
happyReduction_33 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_3  14# happyReduction_34
happyReduction_34 (HappyAbsSyn15  happy_var_3)
        (HappyAbsSyn8  happy_var_2)
        _
         =  HappyAbsSyn18
                 (AddBlank happy_var_2 happy_var_3
        )
happyReduction_34 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happyReduce 8# 15# happyReduction_35
happyReduction_35 ((HappyAbsSyn25  happy_var_8) `HappyStk`
        (HappyAbsSyn24  happy_var_7) `HappyStk`
        (HappyAbsSyn17  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn23  happy_var_4) `HappyStk`
        (HappyAbsSyn22  happy_var_3) `HappyStk`
        (HappyAbsSyn21  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn19
                 (Selecter happy_var_2 happy_var_3 happy_var_4 happy_var_6 happy_var_7 happy_var_8
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happySpecReduce_1  16# happyReduction_36
happyReduction_36 _
         =  HappyAbsSyn20
                 (SortASC
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happySpecReduce_1  16# happyReduction_37
happyReduction_37 _
         =  HappyAbsSyn20
                 (SortDesc
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_38 = happySpecReduce_2  17# happyReduction_38
happyReduction_38 (HappyAbsSyn31  happy_var_2)
        _
         =  HappyAbsSyn21
                 (MaxTrue happy_var_2
        )
happyReduction_38 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_39 = happySpecReduce_0  17# happyReduction_39
happyReduction_39  =  HappyAbsSyn21
                 (MaxFalse
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_40 = happySpecReduce_1  18# happyReduction_40
happyReduction_40 _
         =  HappyAbsSyn22
                 (DistinctTrue
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_41 = happySpecReduce_0  18# happyReduction_41
happyReduction_41  =  HappyAbsSyn22
                 (DistinctFalse
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_42 = happySpecReduce_2  19# happyReduction_42
happyReduction_42 _
        _
         =  HappyAbsSyn23
                 (ColumnALL
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_43 = happySpecReduce_2  19# happyReduction_43
happyReduction_43 (HappyAbsSyn11  happy_var_2)
        _
         =  HappyAbsSyn23
                 (ColumnSingle happy_var_2
        )
happyReduction_43 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_44 = happyReduce 4# 19# happyReduction_44
happyReduction_44 ((HappyAbsSyn23  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn11  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn23
                 (ColumnMultiple happy_var_2 happy_var_4
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_45 = happySpecReduce_2  20# happyReduction_45
happyReduction_45 (HappyAbsSyn28  happy_var_2)
        _
         =  HappyAbsSyn24
                 (WhereTrue happy_var_2
        )
happyReduction_45 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_46 = happySpecReduce_0  20# happyReduction_46
happyReduction_46  =  HappyAbsSyn24
                 (WhereFalse
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_47 = happySpecReduce_2  21# happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_2)
        _
         =  HappyAbsSyn25
                 (PlusTrue happy_var_2
        )
happyReduction_47 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_48 = happySpecReduce_0  21# happyReduction_48
happyReduction_48  =  HappyAbsSyn25
                 (PlusFalse
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_49 = happyReduce 5# 22# happyReduction_49
happyReduction_49 ((HappyAbsSyn28  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn17  happy_var_3) `HappyStk`
        (HappyAbsSyn27  happy_var_2) `HappyStk`
        (HappyAbsSyn17  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn26
                 (JoinClause happy_var_1 happy_var_2 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_50 = happySpecReduce_2  23# happyReduction_50
happyReduction_50 _
        _
         =  HappyAbsSyn27
                 (JoinInner
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_51 = happySpecReduce_2  23# happyReduction_51
happyReduction_51 _
        _
         =  HappyAbsSyn27
                 (JoinLeft
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_52 = happySpecReduce_2  23# happyReduction_52
happyReduction_52 _
        _
         =  HappyAbsSyn27
                 (JoinRight
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_53 = happySpecReduce_2  23# happyReduction_53
happyReduction_53 _
        _
         =  HappyAbsSyn27
                 (JoinFull
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_54 = happySpecReduce_2  23# happyReduction_54
happyReduction_54 _
        _
         =  HappyAbsSyn27
                 (JoinOuter
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_55 = happySpecReduce_3  24# happyReduction_55
happyReduction_55 _
        (HappyAbsSyn28  happy_var_2)
        _
         =  HappyAbsSyn28
                 (BooleanBracket happy_var_2
        )
happyReduction_55 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_56 = happySpecReduce_3  24# happyReduction_56
happyReduction_56 (HappyAbsSyn28  happy_var_3)
        _
        (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn28
                 (BooleanAND happy_var_1 happy_var_3
        )
happyReduction_56 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_57 = happySpecReduce_3  24# happyReduction_57
happyReduction_57 (HappyAbsSyn28  happy_var_3)
        _
        (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn28
                 (BooleanOR happy_var_1 happy_var_3
        )
happyReduction_57 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_58 = happySpecReduce_2  24# happyReduction_58
happyReduction_58 (HappyAbsSyn28  happy_var_2)
        _
         =  HappyAbsSyn28
                 (BooleanNOT happy_var_2
        )
happyReduction_58 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_59 = happySpecReduce_3  24# happyReduction_59
happyReduction_59 (HappyAbsSyn29  happy_var_3)
        _
        (HappyAbsSyn29  happy_var_1)
         =  HappyAbsSyn28
                 (BooleanSubExpression happy_var_1 happy_var_3
        )
happyReduction_59 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_60 = happySpecReduce_1  25# happyReduction_60
happyReduction_60 (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn29
                 (SubBoolean happy_var_1
        )
happyReduction_60 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_61 = happySpecReduce_1  25# happyReduction_61
happyReduction_61 (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn29
                 (SubColumn happy_var_1
        )
happyReduction_61 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_62 = happySpecReduce_1  25# happyReduction_62
happyReduction_62 (HappyTerminal (PT _ (TokenString happy_var_1)))
         =  HappyAbsSyn29
                 (SubString happy_var_1
        )
happyReduction_62 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_63 = happySpecReduce_1  25# happyReduction_63
happyReduction_63 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn29
                 (SubIndex happy_var_1
        )
happyReduction_63 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_64 = happySpecReduce_1  26# happyReduction_64
happyReduction_64 _
         =  HappyAbsSyn30
                 (IndexSingular
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_65 = happySpecReduce_1  26# happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn30
                 (IndexNum happy_var_1
        )
happyReduction_65 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_66 = happySpecReduce_3  26# happyReduction_66
happyReduction_66 _
        (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (IndexBracket happy_var_2
        )
happyReduction_66 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_67 = happySpecReduce_3  26# happyReduction_67
happyReduction_67 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexPlus happy_var_1 happy_var_3
        )
happyReduction_67 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_68 = happySpecReduce_3  26# happyReduction_68
happyReduction_68 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexMinus happy_var_1 happy_var_3
        )
happyReduction_68 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_69 = happySpecReduce_3  26# happyReduction_69
happyReduction_69 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexMult happy_var_1 happy_var_3
        )
happyReduction_69 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_70 = happySpecReduce_3  26# happyReduction_70
happyReduction_70 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexSingularDiv happy_var_1 happy_var_3
        )
happyReduction_70 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_71 = happySpecReduce_3  26# happyReduction_71
happyReduction_71 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexTwoDiv happy_var_1 happy_var_3
        )
happyReduction_71 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_72 = happySpecReduce_3  26# happyReduction_72
happyReduction_72 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexPercent happy_var_1 happy_var_3
        )
happyReduction_72 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_73 = happySpecReduce_3  26# happyReduction_73
happyReduction_73 (HappyAbsSyn30  happy_var_3)
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (IndexExpo happy_var_1 happy_var_3
        )
happyReduction_73 _ _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_74 = happySpecReduce_1  27# happyReduction_74
happyReduction_74 (HappyTerminal (PT _ (TokenPositiveNumber happy_var_1)))
         =  HappyAbsSyn31
                 (PositiveNumber happy_var_1
        )
happyReduction_74 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_75 = happySpecReduce_1  27# happyReduction_75
happyReduction_75 (HappyTerminal (PT _ (TokenNegativeNumber happy_var_1)))
         =  HappyAbsSyn31
                 (NegativeNumber happy_var_1
        )
happyReduction_75 _  = notHappyAtAll 

happyNewToken action sts stk [] =
        happyDoAction 69# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        PT _ TokenINPUT -> cont 1#;
        PT _ TokenOUTPUT -> cont 2#;
        PT _ TokenLET -> cont 3#;
        PT _ TokenBE -> cont 4#;
        PT _ TokenQueriesEnd -> cont 5#;
        PT _ TokenMERGE -> cont 6#;
        PT _ TokenSELECT -> cont 7#;
        PT _ TokenTO -> cont 8#;
        PT _ TokenSTANDARD -> cont 9#;
        PT _ TokenFILE -> cont 10#;
        PT _ TokenPRODUCT -> cont 11#;
        PT _ TokenSORT -> cont 12#;
        PT _ TokenINSERT -> cont 13#;
        PT _ TokenFILL -> cont 14#;
        PT _ TokenDELETE -> cont 15#;
        PT _ TokenCLEAR -> cont 16#;
        PT _ TokenCOLUMN -> cont 17#;
        PT _ TokenCOLUMNS -> cont 18#;
        PT _ TokenROW -> cont 19#;
        PT _ TokenFROMTABLES -> cont 20#;
        PT _ TokenFROM -> cont 21#;
        PT _ TokenJOIN -> cont 22#;
        PT _ TokenINNER -> cont 23#;
        PT _ TokenLEFT -> cont 24#;
        PT _ TokenRIGHT -> cont 25#;
        PT _ TokenFULL -> cont 26#;
        PT _ TokenOUTER -> cont 27#;
        PT _ TokenTABLE -> cont 28#;
        PT _ TokenAS -> cont 29#;
        PT _ TokenWITHLABELS -> cont 30#;
        PT _ TokenNOLABELS -> cont 31#;
        PT _ TokenWHERE -> cont 32#;
        PT _ TokenWITHCONSTRAINT -> cont 33#;
        PT _ TokenAND -> cont 34#;
        PT _ TokenOR -> cont 35#;
        PT _ TokenNOT -> cont 36#;
        PT _ TokenINDEX -> cont 37#;
        PT _ TokenMAX -> cont 38#;
        PT _ TokenPLUSWORD -> cont 39#;
        PT _ TokenDISTINCT -> cont 40#;
        PT _ TokenASC -> cont 41#;
        PT _ TokenDESC -> cont 42#;
        PT _ TokenADDBLANKTO -> cont 43#;
        PT _ TokenEQUAL -> cont 44#;
        PT _ TokenPLUS -> cont 45#;
        PT _ TokenMULTIPLY -> cont 46#;
        PT _ TokenDIVSINGLE -> cont 47#;
        PT _ TokenDIVTWO -> cont 48#;
        PT _ TokenPERCENT -> cont 49#;
        PT _ TokenEXP -> cont 50#;
        PT _ TokenSquigleBracketL -> cont 51#;
        PT _ TokenSquigleBracketR -> cont 52#;
        PT _ TokenDash -> cont 53#;
        PT _ TokenComma -> cont 54#;
        PT _ TokenSquareBracketL -> cont 55#;
        PT _ TokenSquareBracketR -> cont 56#;
        PT _ TokenDot -> cont 57#;
        PT _ TokenBracketL -> cont 58#;
        PT _ TokenBracketR -> cont 59#;
        PT _ (TokenInt happy_dollar_dollar) -> cont 60#;
        PT _ (TokenAlphaLower happy_dollar_dollar) -> cont 61#;
        PT _ (TokenAlphaUpper happy_dollar_dollar) -> cont 62#;
        PT _ (TokenDigits happy_dollar_dollar) -> cont 63#;
        PT _ (TokenFilename happy_dollar_dollar) -> cont 64#;
        PT _ (TokenAlphaNumericString happy_dollar_dollar) -> cont 65#;
        PT _ (TokenPositiveNumber happy_dollar_dollar) -> cont 66#;
        PT _ (TokenNegativeNumber happy_dollar_dollar) -> cont 67#;
        PT _ (TokenString happy_dollar_dollar) -> cont 68#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 69# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(PosnToken)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [PosnToken] -> a
parseError tokens = error ("Parse error at line " ++ (show ln) ++ " column " ++ (show col))
    where
        PT position _ = head tokens
        AlexPn ch ln col = position

data Start = Start Inputs Queries Output deriving Show

data Inputs = InputsCons Input Inputs
                | InputSingle Input
                deriving Show

data Input = Input String TableAssignment deriving Show

data TableAssignment = NoLabels TableName
                | WithLabels TableName ColumnLabels
                deriving Show

data TableName = TableRef String deriving Show

--data Tables = TablesMultiple TableName Tables 
--                | TableSingular TableName
--                deriving Show 

data ColumnLabels = LabelConstructor ColumnLabels2 deriving Show

data ColumnLabels2 = LabelsMultiple String ColumnLabels2 
                | LabelSingular String
                deriving Show

data ColumnReference = AlphaColumn TableName String
                | IntegerColumn TableName Int
                deriving Show

data Output = OutputConstruct TableName OutputType deriving Show

data OutputType = Standard
                | File String
                deriving Show

data Position = Comma Int Int deriving Show

data Queries = QueryLet TableAssignment Query Queries
                | QueryEnd
                deriving Show

data TableExpression = SingleTable TableName
                | TableLetQuery TableAssignment Query
                | TableLetJoin TableAssignment JoinClause
                | SingleTableExpression TableExpression
                deriving Show

data Query = Merge TableName TableName BooleanExpression
                | SelecterQuery Selection
                | Product TableName TableName
                | Sort TableName SortClause
                | Insert String TableName Position
                | Fill String TableName Axis
                | Delete TableName Axis
                | Clear TableName Position
                | AddBlank TableName Axis
                deriving Show


data Axis = ColumnInt Int
                | ColumnAlpha String
                | Row Int
                deriving Show


data Selection = Selecter MaxClause Distinct ColumnChoice TableExpression WhereClause PlusClause deriving Show

data SortClause = SortASC | SortDesc deriving Show

data MaxClause = MaxTrue Number
                | MaxFalse
                deriving Show

data Distinct = DistinctTrue
                | DistinctFalse
                deriving Show

data ColumnChoice = ColumnALL 
                | ColumnSingle ColumnReference
                | ColumnMultiple ColumnReference ColumnChoice
                deriving Show

data WhereClause = WhereTrue BooleanExpression
                | WhereFalse 
                deriving Show

data PlusClause = PlusTrue Selection
                | PlusFalse
                deriving Show

data JoinClause = JoinClause TableExpression JoinOperator TableExpression BooleanExpression deriving Show

data JoinOperator = JoinInner
                | JoinLeft
                | JoinRight
                | JoinFull
                | JoinOuter
                deriving Show

data BooleanExpression = BooleanBracket BooleanExpression
                | BooleanAND BooleanExpression BooleanExpression
                | BooleanOR BooleanExpression BooleanExpression
                | BooleanNOT BooleanExpression
                | BooleanSubExpression SubExpression SubExpression
                deriving Show

data SubExpression = SubBoolean BooleanExpression
                | SubColumn ColumnReference
                | SubString String
                | SubIndex IndexExpression
                deriving Show

data IndexExpression = IndexSingular
                | IndexNum Number
                | IndexBracket IndexExpression
                | IndexPlus IndexExpression IndexExpression
                | IndexMinus IndexExpression IndexExpression
                | IndexMult IndexExpression IndexExpression
                | IndexSingularDiv IndexExpression IndexExpression
                | IndexTwoDiv IndexExpression IndexExpression
                | IndexPercent IndexExpression IndexExpression
                | IndexExpo IndexExpression IndexExpression
                deriving Show


data Number = PositiveNumber String
                | NegativeNumber String
                deriving (Show, Read)
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
