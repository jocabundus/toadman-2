'=============================================================================
'                           TOADMAN 1 TO TOADMAN 2
'                              LEVEL CONVERTER
'                            Created by Joe King
'=============================================================================

SCREEN 0
CLS

PRINT "Toadman 1 to Toadman 2 Level converter"
PRINT "2000 Created by Joe King                  Path might be needed also"
INPUT "Enter name of Toadman 1 level/", nm1$
INPUT "Enter name of Toadman 2 level/", nm2$

  DIM mz$(40)
  DIM mz%(50, 50)

IF RIGHT$(nm1$, 4) <> ".td1" THEN nm1$ = nm1$ + ".td1"
IF RIGHT$(nm2$, 4) <> ".td2" THEN nm2$ = nm2$ + ".td2"
PRINT "Converting Toadman 1 map ", nm1$
PRINT "to Toadman 2 map ", nm2$

OPEN nm1$ FOR INPUT AS #1
FOR x% = 1 TO 40
  INPUT #1, mz$(x%)
NEXT x%
CLOSE #1


OuterBorder% = 215

FOR y% = 1 TO 40
FOR x% = 1 TO 40
  m$ = MID$(mz$(y%), x%, 1)
  IF m$ = "1" THEN mz%(x%, y%) = 78
  IF m$ = "2" THEN mz%(x%, y%) = 13
  IF m$ = "3" THEN mz%(x%, y%) = 14
  IF m$ = "4" THEN mz%(x%, y%) = 15
  IF m$ = "5" THEN mz%(x%, y%) = 16
  IF m$ = "L" THEN mz%(x%, y%) = 103
  IF m$ = "l" THEN mz%(x%, y%) = 104
  IF m$ = "6" THEN mz%(x%, y%) = 17
  IF m$ = "7" THEN mz%(x%, y%) = 18
  IF m$ = "8" THEN mz%(x%, y%) = 19
  IF m$ = "9" THEN mz%(x%, y%) = 20
  IF m$ = "0" THEN mz%(x%, y%) = 21
  IF m$ = "G" THEN mz%(x%, y%) = 22
  IF m$ = "V" THEN mz%(x%, y%) = 23
  IF m$ = "A" THEN mz%(x%, y%) = 26
  IF m$ = "a" THEN mz%(x%, y%) = 27
  IF m$ = "Z" THEN mz%(x%, y%) = 28
  IF m$ = "z" THEN mz%(x%, y%) = 29
  IF m$ = "n" THEN mz%(x%, y%) = 211
  IF m$ = "N" THEN mz%(x%, y%) = 666
  IF m$ = "M" THEN mz%(x%, y%) = 672
  IF m$ = "#" THEN mz%(x%, y%) = 678
  IF m$ = "_" THEN mz%(x%, y%) = 130
  IF m$ = "=" THEN mz%(x%, y%) = 132
  IF m$ = "+" THEN mz%(x%, y%) = 134
  IF m$ = "*" THEN mz%(x%, y%) = 227
  IF m$ = "K" THEN mz%(x%, y%) = 106
  IF m$ = "k" THEN mz%(x%, y%) = 108
  IF m$ = "R" THEN mz%(x%, y%) = 110
  IF m$ = "D" THEN mz%(x%, y%) = 118
  IF m$ = "d" THEN mz%(x%, y%) = 120
  IF m$ = "r" THEN mz%(x%, y%) = 122
  IF m$ = "W" THEN mz%(x%, y%) = 215
  IF m$ = "T" THEN mz%(x%, y%) = 48
  IF m$ = "t" THEN mz%(x%, y%) = 49
  IF m$ = "Y" THEN mz%(x%, y%) = 52
  IF m$ = "y" THEN mz%(x%, y%) = 53
  IF m$ = "S" THEN mz%(x%, y%) = 50
  IF m$ = "s" THEN mz%(x%, y%) = 51
  IF m$ = "v" THEN mz%(x%, y%) = 24
  IF m$ = "-" THEN mz%(x%, y%) = 25
  IF m$ = "B" THEN mz%(x%, y%) = 1
  IF m$ = "b" THEN mz%(x%, y%) = 5
NEXT x%
NEXT y%

FOR y% = 1 TO 50
FOR x% = 1 TO 50
  IF x% > 40 OR y% > 40 THEN mz%(x%, y%) = 215
NEXT x%
NEXT y%

mz%(0, 0) = OuterBorder%
DEF SEG = VARSEG(mz%(0, 0))
BSAVE nm2$, VARPTR(mz%(0, 0)), 5202
DEF SEG

PRINT "Done."
PRINT "Thanks for using Toadman 1 to Toadman 2 level converter."

