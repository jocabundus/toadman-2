'=============================================================================
'                             TOADMAN 2
'                           LEVEL BUILDER
'=============================================================================

'KEYS
'--------
'CURSOR KEYS = MOVE AROUND
'KEYPAD      = SCROLL AROUND
'ENTER       = GO TO SPRITE LIST
'SPACE       = PUT SPRITE
'T           = SWITCH ON/OFF TRACE MODE
'R           = SWITCH ON/OFF RANDOM MODE
'TAB         = COPY TILE
'F1          = GO TO MAIN
'IN THE SPRITE LIST
'SPACE       = SELECT RANDOM TILE (can have up to 20)
'ENTER       = CHOOSE TILE
'ESC         = GO BACK TO EDIT MODE


DECLARE SUB ChooseBorder ()
DECLARE SUB Save ()
DECLARE SUB Load ()
DECLARE SUB Main ()
DECLARE SUB SpriteList ()
 
  COMMON SHARED eps%, sx%, sy%, PutUpGrid%, sn%, OuterBorder%, rn%
 
  DIM SHARED Map%(50, 50)
  DIM SHARED sl%(400)
  DIM SHARED rand%(20)

RANDOMIZE TIMER

SCREEN 13


'***** Load the sprites *****
'=============================================================================
DEFINT A-Z
TYPE HUES
  Red AS INTEGER
  grn AS INTEGER
  blu AS INTEGER
END TYPE
DIM Pal(255) AS HUES
DEF SEG = VARSEG(Pal(0))
BLOAD "default.PAL", 0
OUT &H3C8, 0
FOR atrib = 0 TO 255
 OUT &H3C9, Pal(atrib).Red
 OUT &H3C9, Pal(atrib).grn
 OUT &H3C9, Pal(atrib).blu
NEXT atrib
FileName$ = "TOADMAN2.SPR"
OPEN FileName$ FOR BINARY AS #1
filesize& = LOF(1)
CLOSE #1
bytes = (filesize& - 7) \ 2 - 1
REDIM SHARED sprites%(bytes)
DEF SEG = VARSEG(sprites%(0))
BLOAD FileName$, 0
spritewidth = sprites%(0) \ 8
spriteheight = sprites%(1)
xsprites = 319 \ (spritewidth + 1)
xend = spritewidth * (xsprites - 1) + xsprites
eps% = ((spritewidth * spriteheight) \ 2) + 3
'=============================================================================

'***** Read the spritelist data *****
'=============================================================================
FOR n% = 1 TO 400
READ sl%(n%)
NEXT n%
'=============================================================================

'***** Starting up *****
'=============================================================================
SCREEN 13

  GridX% = 0: GridY% = 0
  x% = 1: y% = 1
  sx% = 1: sy% = 1
  tr% = 0: rt% = 1
  rd% = 0: dr% = 1

  PutUpGrid% = 1
'=============================================================================

'***** The main code *****
'=============================================================================

DO

  IF PutUpGrid% THEN
    FOR yy% = 1 TO 20
    FOR xx% = 1 TO 40
      PUT (xx% * 8 - 8, yy% * 8 - 8), sprites%(eps% * Map%(xx% + GridX%, yy% + GridY%)), PSET
    NEXT xx%
    NEXT yy%
    PutUpGrid% = 0
  END IF

  PUT (x% * 8 - 8, y% * 8 - 8), sprites%(eps% * 35), PSET
  LOCATE 22, 1
  PRINT "X ="; x% + GridX%; "Y ="; y% + GridY%
  IF tr% = 1 THEN PRINT "TRACE ON  ";  ELSE PRINT "TRACE OFF ";
  IF rd% = 1 THEN PRINT "RANDOM ON  " ELSE PRINT "RANDOM OFF "

  DO
  a$ = INKEY$
  LOOP UNTIL a$ <> ""
  PUT (x% * 8 - 8, y% * 8 - 8), sprites%(eps% * Map%(x% + GridX%, y% + GridY%)), PSET
  IF a$ = CHR$(0) + "M" THEN x% = x% + 1
  IF a$ = CHR$(0) + "K" THEN x% = x% - 1
  IF a$ = CHR$(0) + "P" THEN y% = y% + 1
  IF a$ = CHR$(0) + "H" THEN y% = y% - 1
  IF a$ = CHR$(0) + "S" THEN Map%(x% + GridX%, y% + GridY%) = 0
  IF a$ = "6" THEN GridX% = GridX% + 1: PutUpGrid% = 1
  IF a$ = "4" THEN GridX% = GridX% - 1: PutUpGrid% = 1
  IF a$ = "2" THEN GridY% = GridY% + 1: PutUpGrid% = 1
  IF a$ = "8" THEN GridY% = GridY% - 1: PutUpGrid% = 1
  IF a$ = CHR$(9) THEN sn% = Map%(x% + GridX%, y% + GridY%)
  IF a$ = CHR$(0) + ";" THEN Main: DO: LOOP UNTIL INKEY$ = ""
  IF a$ = " " THEN Map%(x% + GridX%, y% + GridY%) = sn%
  IF a$ = CHR$(13) THEN SpriteList
  IF UCASE$(a$) = "T" THEN SWAP tr%, rt%
  IF UCASE$(a$) = "R" THEN SWAP rd%, dr%
    IF rd% = 1 THEN
      ns% = INT((rn% - 1) * RND(1)) + 1
      sn% = rand%(ns%)
    END IF
  IF x% < 1 THEN x% = 1
  IF x% > 40 THEN x% = 40
  IF y% < 1 THEN y% = 1
  IF y% > 20 THEN y% = 20
  IF GridX% < 0 THEN GridX% = 0
  IF GridX% > 10 THEN GridX% = 10
  IF GridY% < 0 THEN GridY% = 0
  IF GridY% > 30 THEN GridY% = 30

  IF tr% = 1 THEN Map%(x% + GridX%, y% + GridY%) = sn%

LOOP
'=============================================================================


'***** The spritelist data *****
'=============================================================================
DATA 001,005,000,065,066,067,068,102,000,211,666,672,678,684,130,132,134,227,000,136,138,000,000,013,017,022,027,000,000,000,000,000,000,000,000,000,054,055,056,064
DATA 071,072,073,074,075,076,077,078,079,080,081,082,083,084,085,086,087,088,089,090,091,092,014,018,023,028,000,000,000,000,725,726,727,728,729,730,057,058,059,060
DATA 106,108,110,112,114,116,118,120,122,124,126,128,093,094,095,096,097,098,099,100,101,000,015,019,024,029,000,000,717,718,719,720,721,722,723,724,030,061,062,063
DATA 141,143,145,147,149,151,153,155,157,159,161,163,165,167,169,000,000,000,048,049,050,000,016,020,025,171,731,732,701,702,703,704,705,706,707,708,709,710,711,712
DATA 176,179,182,185,188,191,215,219,223,231,195,200,000,000,000,000,000,000,051,052,053,103,104,021,026,173,733,734,736,737,738,739,740,741,742,743,744,745,746,747
DATA 246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,000,000,713,714,715,716
DATA 281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,748,749,750,751,752,753
DATA 316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,754,755,756,757,758,759
DATA 351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,760,761,762,763,764,765
DATA 386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,000,000,766,767,768,769
'=============================================================================

DEFSNG A-Z
SUB ChooseBorder

CLS
COLOR 15
PRINT "Which kind of border?"
PRINT "  Inner Border"
PRINT "  Outer Border"
x% = 2
io% = 0: ob% = 0

DO
  COLOR 9
  LOCATE x%, 1: PRINT ""
  DO
  a$ = INKEY$
  LOOP UNTIL a$ <> ""
  LOCATE x%, 1: PRINT " "
  IF a$ = CHR$(0) + "P" THEN x% = x% + 1
  IF a$ = CHR$(0) + "H" THEN x% = x% - 1
  IF a$ = CHR$(27) THEN EXIT SUB
  IF a$ = CHR$(13) THEN IF x% = 2 THEN ib% = 1: EXIT DO
  IF a$ = CHR$(13) THEN IF x% = 3 THEN ob% = 1: EXIT DO
  IF x% > 3 THEN x% = 2
  IF x% < 2 THEN x% = 3
LOOP

CLS
bd% = 0

DO
FOR y% = 0 TO 23 STEP 8
FOR x% = 0 TO 23 STEP 8
  PUT (x%, y%), sprites%(eps% * bd%), PSET
NEXT x%
NEXT y%

LOCATE 10, 1
COLOR 15
PRINT "Press  "
PRINT "Press Esc to go back"
PRINT "Press Enter to Choose"

DO
  a$ = INKEY$
LOOP UNTIL a$ <> ""
IF a$ = CHR$(0) + "M" THEN bd% = bd% + 1
IF a$ = CHR$(0) + "K" THEN bd% = bd% - 1
IF a$ = CHR$(27) THEN EXIT SUB
IF ib% = 1 THEN
  IF a$ = CHR$(13) THEN
    FOR y% = 1 TO 50
    FOR x% = 1 TO 50
      IF x% = 1 OR y% = 1 OR x% = 50 OR y% = 50 THEN Map%(x%, y%) = bd%
    NEXT x%
    NEXT y%
    EXIT SUB
  END IF
END IF
IF ob% = 1 AND a$ = CHR$(13) THEN OuterBorder% = bd%: EXIT SUB
IF bd% < 0 THEN bd% = 769
IF bd% > 769 THEN bd% = 0
LOOP

END SUB

SUB Load

CLS

COLOR 15
PRINT "Are you sure you want to Load?"
PRINT "  Yes"
PRINT "  No"
x% = 2

DO
  COLOR 9
  LOCATE x%, 1: PRINT ""
  DO
  a$ = INKEY$
  LOOP UNTIL a$ <> ""
  LOCATE x%, 1: PRINT " "
  IF a$ = CHR$(0) + "P" THEN x% = x% + 1
  IF a$ = CHR$(0) + "H" THEN x% = x% - 1
  IF a$ = CHR$(27) THEN EXIT SUB
  IF x% > 3 THEN x% = 2
  IF x% < 2 THEN x% = 3
  IF a$ = CHR$(13) THEN
    IF x% = 2 THEN
      CLS
      COLOR 15
      INPUT "Enter Name?", nm$
      CLS
      PRINT "Press escape to go back, or any other   key to load"
      DO
      a$ = INKEY$
      LOOP UNTIL a$ <> ""
      IF a$ = CHR$(27) THEN EXIT SUB
      CLS
      PRINT "Loading"
      DEF SEG = VARSEG(Map%(0, 0))
      BLOAD nm$ + ".td2", VARPTR(Map%(0, 0))
      DEF SEG
      OuterBorder% = Map%(0, 0)
      PutUpGrid% = 1
      CLS
      PRINT "Loaded"
      SLEEP
      EXIT SUB
    END IF
    IF x% = 3 THEN EXIT SUB
  END IF
LOOP

END SUB

SUB Main

CLS
COLOR 15
PRINT "  Choose Border"
PRINT "  Save Map"
PRINT "  Load Map"
PRINT "  Exit Program"
x% = 1

DO
LOCATE x%, 1
COLOR 9
PRINT ""
DO
  a$ = INKEY$
LOOP UNTIL a$ <> ""
LOCATE x%, 1: PRINT " "
IF a$ = CHR$(0) + "P" THEN x% = x% + 1
IF a$ = CHR$(0) + "H" THEN x% = x% - 1
IF a$ = CHR$(27) THEN PutUpGrid% = 1: EXIT SUB
IF a$ = CHR$(13) THEN
  IF x% = 1 THEN ChooseBorder: PutUpGrid% = 1: EXIT SUB
  IF x% = 2 THEN Save: PutUpGrid% = 1: EXIT SUB
  IF x% = 3 THEN Load: PutUpGrid% = 1: EXIT SUB
  IF x% = 4 THEN
  CLS
  COLOR 15
  PRINT "Press escape to go back, or any other   key to exit"
  DO
    a$ = INKEY$
  LOOP UNTIL a$ <> ""
  IF a$ = CHR$(27) THEN PutUpGrid% = 1: EXIT SUB
  END
  END IF
END IF
IF x% < 1 THEN x% = 4
IF x% > 4 THEN x% = 1
LOOP


END SUB

SUB Save

CLS

COLOR 15
PRINT "Are you sure you want to save?"
PRINT "  Yes"
PRINT "  No"
x% = 2

DO
  COLOR 9
  LOCATE x%, 1: PRINT ""
  DO
  a$ = INKEY$
  LOOP UNTIL a$ <> ""
  LOCATE x%, 1: PRINT " "
  IF a$ = CHR$(0) + "P" THEN x% = x% + 1
  IF a$ = CHR$(0) + "H" THEN x% = x% - 1
  IF a$ = CHR$(27) THEN EXIT SUB
  IF x% > 3 THEN x% = 2
  IF x% < 2 THEN x% = 3
  IF a$ = CHR$(13) THEN
    IF x% = 2 THEN
      CLS
      COLOR 15
      INPUT "Enter Name?", nm$
      CLS
      PRINT "Press escape to go back, or any other   key to save"
      DO
      a$ = INKEY$
      LOOP UNTIL a$ <> ""
      IF a$ = CHR$(27) THEN EXIT SUB
      CLS
      PRINT "Saving"
      Map%(0, 0) = OuterBorder%
      DEF SEG = VARSEG(Map%(0, 0))
      BSAVE nm$ + ".td2", VARPTR(Map%(0, 0)), 5202
      DEF SEG
      CLS
      PRINT "Saved"
      SLEEP
      EXIT SUB
    END IF
    IF x% = 3 THEN EXIT SUB
  END IF
LOOP

END SUB

SUB SpriteList

CLS

x% = 1: y% = 1
FOR n% = 1 TO 400
PUT (x% * 8 - 8, y% * 8 - 8), sprites%(eps% * sl%(n%)), PSET
x% = x% + 1
IF x% > 40 THEN x% = 1: y% = y% + 1
NEXT n%
r% = 1

DO

  PUT (sx% * 8 - 8, sy% * 8 - 8), sprites%(eps% * 35), PSET
 
  DO
    a$ = INKEY$
  LOOP UNTIL a$ <> ""
    PUT (sx% * 8 - 8, sy% * 8 - 8), sprites%(eps% * sl%((40 * sy%) - 40 + sx%)), PSET
    IF a$ = CHR$(0) + "M" THEN sx% = sx% + 1
    IF a$ = CHR$(0) + "K" THEN sx% = sx% - 1
    IF a$ = CHR$(0) + "P" THEN sy% = sy% + 1
    IF a$ = CHR$(0) + "H" THEN sy% = sy% - 1
    IF a$ = CHR$(27) THEN PutUpGrid% = 1: EXIT SUB
    IF a$ = CHR$(13) THEN sn% = sl%((40 * sy%) - 40 + sx%): PutUpGrid% = 1: EXIT SUB
    IF a$ = " " THEN rand%(r%) = sl%((40 * sy%) - 40 + sx%): r% = r% + 1: rn% = r%
    IF r% > 20 THEN r% = 20
    IF sy% > 10 THEN sy% = 10
    IF sy% < 1 THEN sy% = 1
    IF sx% > 40 THEN sx% = 40
    IF sx% < 1 THEN sx% = 1
LOOP

END SUB

