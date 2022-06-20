'=============================================================================
'                            Delta Code Presents
'                            *****TOADMAN 2*****
'                            ****SPACE SHOCK****
'                          2000 Created by Joe King
'=============================================================================

'***** You need to change the speed constants below for good gameplay *****
'***** Change OnePlayerMode% to 1 for one player mode *****

DECLARE SUB BadguyShoot1 ()
DECLARE SUB Credits ()
DECLARE SUB DropMine (z%)
DECLARE SUB BDead (z%)
DECLARE SUB Restart ()
DECLARE SUB Dead (z%)
DECLARE SUB Bazooka (z%)
DECLARE SUB LeaveTrail (z%)
DECLARE SUB BadguyShoot (z%)
DECLARE SUB MoveBadguys ()
DECLARE SUB Alive (z%, x%, y%)
DECLARE SUB Intro ()
DECLARE SUB Hit (z%)
DECLARE SUB Fiddle (z%)
DECLARE SUB Shoot (z%)
DECLARE SUB Status ()
DECLARE SUB CheckMap ()
DECLARE SUB Box (x1%, y1%, x2%, y2%)
DECLARE SUB Engine ()
DECLARE SUB AddTimers ()
DECLARE SUB load (nm$)
 
  COMMON SHARED eps%, mc%, cm%, OuterBorder%, ss%, ld%, Coun%, Count1&, Count2&
 
'***** The speed constants *****
'=================
CONST IntroSpeed& = 15000  '***** The Intro Speed
CONST ASpeed& = 1          '***** The speed of the animation
CONST BSpeed& = 5          '***** The speed at which the frozen toads move
CONST OnePlayerMode% = 0   '***** Change it to 1 for one player mode
'=================
 
 
  DIM SHARED Map%(50, 50)
  DIM SHARED BMap%(50, 50, 3)
  DIM SHARED Life%(2)
  DIM SHARED Blife%(3)
  DIM SHARED Ammo%(2)
  DIM SHARED KeyCard%(6, 2)
  DIM SHARED kl%(2)
  DIM SHARED px%(2)
  DIM SHARED py%(2)
  DIM SHARED opx%(2)
  DIM SHARED opy%(2)
  DIM SHARED bx%(2)
  DIM SHARED by%(2)
  DIM SHARED obx%(2)
  DIM SHARED oby%(2)
  DIM SHARED ps%(2)       '***** The position of the toads (left or right)
  DIM SHARED bps%(2)
  DIM SHARED tm%(7)       '***** The animation timers
 
  DIM SHARED canx%(500)   '***** The canister
  DIM SHARED cany%(500)   '***** Explosion
  DIM SHARED cant%(500)   '***** variables
 
  DIM SHARED Weapon%(2)
  DIM SHARED ht%(2)
 
  DIM SHARED tx1%(2)  '***** The
  DIM SHARED ty1%(2)  '***** Transporter
  DIM SHARED tx2%(2)  '***** variables
  DIM SHARED ty2%(2)  '*****
 
  DIM SHARED ax%(2)   '***** Things for when
  DIM SHARED ay%(2)   '***** the frozen
  DIM SHARED al%(3)   '***** toads come
  DIM SHARED at%(2)   '***** to life

  DIM SHARED eh%(2)
  DIM SHARED Baz%(2)
  DIM SHARED BazAmmo%(2)
  DIM SHARED BazOn%(2)
  DIM SHARED BazX%(2)
  DIM SHARED BazY%(2)

  DIM SHARED ExtraAmmo%(2)

  DIM SHARED DeadX%(2)
  DIM SHARED DeadY%(2)
  DIM SHARED DeadT%(2)
  DIM SHARED BDeadX%(3)
  DIM SHARED BDeadY%(3)
  DIM SHARED BDeadT%(3)

  DIM SHARED Sweeper%(2)
  DIM SHARED Mines%(2)
  DIM SHARED Poison%(2)
  DIM SHARED Bife%(2)

  RANDOMIZE TIMER

SCREEN 13
'$DYNAMIC

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
REDIM SHARED sprites(bytes)
DEF SEG = VARSEG(sprites(0))
BLOAD FileName$, 0
spritewidth = sprites(0) \ 8
spriteheight = sprites(1)
xsprites = 319 \ (spritewidth + 1)
xend = spritewidth * (xsprites - 1) + xsprites
eps% = ((spritewidth * spriteheight) \ 2) + 3
'=============================================================================

Intro

Life%(1) = 100: Life%(2) = 100
IF OnePlayerMode% = 1 THEN Life%(1) = 500
Ammo%(1) = 150: Ammo%(2) = 150
ss% = 4     '***** The size of the screen

Box 54, 30, 145, 121
Box 180, 30, 271, 121

Status

DO

  DO
    Engine
    ht%(1) = 0: ht%(2) = 0
    a$ = INKEY$
  LOOP UNTIL a$ <> ""
    opx%(1) = px%(1): opy%(1) = py%(1)
    opx%(2) = px%(2): opy%(2) = py%(2)
    IF Coun% = 1 THEN
      IF UCASE$(a$) = "Y" THEN Restart
      IF UCASE$(a$) = "N" THEN RUN
    END IF
    IF a$ = "+" THEN ss% = ss% + 1: Box 54, 30, 145, 121: Box 180, 30, 271, 121
    IF a$ = "-" THEN ss% = ss% - 1: Box 54, 30, 145, 121: Box 180, 30, 271, 121
    IF DeadT%(1) = 0 THEN
      IF BazOn%(1) = 0 THEN
        IF UCASE$(a$) = "D" THEN px%(1) = px%(1) + 1: ps%(1) = 2: LeaveTrail 1
        IF UCASE$(a$) = "A" THEN px%(1) = px%(1) - 1: ps%(1) = 0: LeaveTrail 1
        IF UCASE$(a$) = "S" THEN py%(1) = py%(1) + 1: LeaveTrail 1
        IF UCASE$(a$) = "W" THEN py%(1) = py%(1) - 1: LeaveTrail 1
      ELSE
        IF UCASE$(a$) = "D" THEN BazX%(1) = BazX%(1) + 1
        IF UCASE$(a$) = "A" THEN BazX%(1) = BazX%(1) - 1
        IF UCASE$(a$) = "S" THEN BazY%(1) = BazY%(1) + 1
        IF UCASE$(a$) = "W" THEN BazY%(1) = BazY%(1) - 1
      END IF
    IF UCASE$(a$) = "Q" THEN Shoot 1
    IF a$ = "1" THEN Hit 1
    IF a$ = "2" THEN DropMine 1
    IF a$ = "~" THEN Fiddle 1
    IF a$ = CHR$(9) AND Baz%(1) = 1 THEN Bazooka 1
    END IF
    IF DeadT%(2) = 0 THEN
      IF BazOn%(2) = 0 THEN
        IF a$ = CHR$(0) + "M" THEN px%(2) = px%(2) + 1: ps%(2) = 2: LeaveTrail 2: LeaveTrail 3
        IF a$ = CHR$(0) + "K" THEN px%(2) = px%(2) - 1: ps%(2) = 0: LeaveTrail 2: LeaveTrail 3
        IF a$ = CHR$(0) + "P" THEN py%(2) = py%(2) + 1: LeaveTrail 2
        IF a$ = CHR$(0) + "H" THEN py%(2) = py%(2) - 1: LeaveTrail 2
      ELSE
        IF a$ = CHR$(0) + "M" THEN BazX%(2) = BazX%(2) + 1
        IF a$ = CHR$(0) + "K" THEN BazX%(2) = BazX%(2) - 1
        IF a$ = CHR$(0) + "P" THEN BazY%(2) = BazY%(2) + 1
        IF a$ = CHR$(0) + "H" THEN BazY%(2) = BazY%(2) - 1
      END IF
    IF a$ = CHR$(13) THEN Shoot 2
    IF a$ = "]" THEN Hit 2
    IF a$ = "[" THEN DropMine 2
    IF a$ = "|" THEN Fiddle 2
    IF a$ = "\" AND Baz%(2) = 1 THEN Bazooka 2
    END IF
    IF a$ = CHR$(27) THEN END
    CheckMap
    IF OnePlayerMode% = 1 THEN
      DO
      LOOP UNTIL INKEY$ = ""
    END IF
LOOP

REM $STATIC
DEFSNG A-Z
SUB AddTimers

'***** Add the animation timers *****
'=============================================================================
tm%(2) = tm%(2) + 1
tm%(3) = tm%(3) + 1
tm%(4) = tm%(4) + 1
tm%(5) = tm%(5) + 1
tm%(7) = tm%(7) + 1
IF tm%(2) > 1 THEN tm%(2) = 0
IF tm%(3) > 2 THEN tm%(3) = 0
IF tm%(4) > 3 THEN tm%(4) = 0
IF tm%(5) > 5 THEN tm%(5) = 0
IF tm%(7) > 4 THEN tm%(7) = 0
IF tm%(6) <> 0 THEN tm%(6) = tm%(6) + 1
IF tm%(6) > 15 THEN tm%(6) = 0

  IF cm% >= mc% THEN
    FOR n% = 1 TO mc%
      Map%(canx%(n%), cany%(n%)) = 0
    NEXT n%
    mc% = 0: cm% = 0
  END IF

  FOR n% = 1 TO mc%
    cant%(n%) = cant%(n%) + 1
    IF cant%(n%) = 17 THEN Map%(canx%(n%), cany%(n%)) = 0: cm% = cm% + 1: PSET (canx%(n%), cany%(n%)), 0
    IF cant%(n%) < 17 THEN PSET (canx%(n%), cany%(n%)), 14

    IF cant%(n%) = 2 THEN
      FOR y% = -1 TO 1
      FOR x% = -1 TO 1
        IF canx%(n%) + x% = px%(1) AND cany%(n%) + y% = py%(1) THEN Life%(1) = Life%(1) - 30: ht%(1) = 1: Status
        IF canx%(n%) + x% = px%(2) AND cany%(n%) + y% = py%(2) THEN Life%(2) = Life%(2) - 30: ht%(2) = 1: Status
        m% = Map%(canx%(n%) + x%, cany%(n%) + y%)
        IF m% = 104 THEN Alive 1, canx%(n%) + x%, cany%(n%) + y%
        IF m% = 103 THEN Alive 2, canx%(n%) + x%, cany%(n%) + y%
        IF m% = 169 THEN
          Map%(canx%(n%) + x%, cany%(n%) + y%) = 420
          mc% = mc% + 1
          IF mc% > 500 THEN mc% = 500
          cant%(mc%) = 1
          canx%(mc%) = canx%(n%) + x%
          cany%(mc%) = cany%(n%) + y%
        END IF
       
        IF m% > 140 AND m% < 147 THEN Map%(canx%(n%) + x%, cany%(n%) + y%) = Map%(canx%(n%) + x%, cany%(n%) + y%) + 6
        IF m% > 175 AND m% < 185 THEN Map%(canx%(n%) + x%, cany%(n%) + y%) = Map%(canx%(n%) + x%, cany%(n%) + y%) + 9
        IF m% > 245 AND m% < 420 THEN
          Map%(canx%(n%) + x%, cany%(n%) + y%) = Map%(canx%(n%) + x%, cany%(n%) + y%) + 35
          IF Map%(canx%(n%) + x%, cany%(n%) + y%) > 420 THEN Map%(canx%(n%) + x%, cany%(n%) + y%) = 0
        END IF

      NEXT x%
      NEXT y%
    END IF

  NEXT n%

  FOR z% = 1 TO 2
    IF al%(z%) = 1 THEN
      at%(z%) = at%(z%) + 1
      IF z% = 1 THEN Map%(ax%(1), ay%(1)) = 595 + at%(1)
      IF z% = 2 THEN Map%(ax%(2), ay%(2)) = 606 + at%(2)
      IF at%(z%) > 9 THEN at%(z%) = 0: al%(z%) = 2: Map%(ax%(z%), ay%(z%)) = 0: bx%(z%) = ax%(z%): by%(z%) = ay%(z%)
    END IF
  NEXT z%

  FOR z% = 1 TO 2
    IF DeadT%(z%) > 0 THEN
      Map%(DeadX%(z%), DeadY%(z%)) = 420 + (z% * 35) + DeadT%(z%)
      DeadT%(z%) = DeadT%(z%) + 1
      IF DeadT%(z%) > 30 THEN
        LOCATE 1, 14
        COLOR 14
        g% = 0
        IF Life%(1) < 1 AND Life%(2) < 1 THEN PRINT " NOBODY WINS! ": g% = 1
        IF g% = 0 AND z% = 1 THEN PRINT "PLAYER 2 WINS!"
        IF g% = 0 AND z% = 2 THEN PRINT "PLAYER 1 WINS!"
        LOCATE 2, 14
        COLOR 9
        PRINT "CONTINUE (Y/N)"
        Coun% = 1
        Map%(DeadX%(z%), DeadY%(z%)) = 0
        DeadT%(z%) = -1
        ht%(z%) = 0
      END IF
    END IF
  NEXT z%

  FOR z% = 1 TO 2
    IF BDeadT%(z%) > 0 THEN
      Map%(BDeadX%(z%), BDeadY%(z%)) = 490 + (z% * 35) + BDeadT%(z%)
      BDeadT%(z%) = BDeadT%(z%) + 1
      IF BDeadT%(z%) > 30 THEN
        Map%(BDeadX%(z%), BDeadY%(z%)) = 0
        BDeadT%(z%) = -1
      END IF
    END IF
 
  IF tm%(5) = 0 AND Poison%(z%) = 1 THEN ht%(z%) = 1: Life%(z%) = Life%(z%) - 1
 
  NEXT z%

'=============================================================================

END SUB

SUB Alive (z%, x%, y%)

'***** Make the frozen toads come to life *****
'=============================================================================

  ax%(z%) = x%: ay%(z%) = y%: al%(z%) = 1: at%(z%) = 0
  Blife%(z%) = 200
  AddTimers

'=============================================================================

END SUB

SUB BadguyShoot (z%)

'***** The shooting sub for the badguys *****
'=============================================================================

IF bps%(z%) = 0 THEN dr% = -1 ELSE dr% = 1
x% = bx%(z%)

DO
  x% = x% + dr%
  IF x% = px%(1) AND by%(z%) = py%(1) THEN Life%(1) = Life%(1) - 5: ht%(1) = 1: EXIT DO
  IF x% = px%(2) AND by%(z%) = py%(2) THEN Life%(2) = Life%(2) - 5: ht%(2) = 1: EXIT DO
  IF Map%(x%, by%(z%)) = 48 THEN Map%(x%, by%(z%)) = 52
  IF Map%(x%, by%(z%)) = 49 THEN Map%(x%, by%(z%)) = 53
  IF Map%(x%, by%(z%)) = 50 THEN Map%(x%, by%(z%)) = 48
  IF Map%(x%, by%(z%)) = 51 THEN Map%(x%, by%(z%)) = 49
  IF Map%(x%, by%(z%)) = 26 THEN Map%(x%, by%(z%)) = 28
  IF Map%(x%, by%(z%)) = 27 THEN Map%(x%, by%(z%)) = 29
  IF Map%(x%, by%(z%)) = 23 THEN Map%(x%, by%(z%)) = 0
  IF Map%(x%, by%(z%)) = 104 THEN Map%(x%, by%(z%)) = 596: Alive 1, x%, by%(z%)
  IF Map%(x%, by%(z%)) = 103 THEN Map%(x%, by%(z%)) = 606: Alive 2, x%, by%(z%)
  IF Map%(x%, by%(z%)) THEN EXIT DO
  Map%(x%, by%(z%)) = 33
LOOP

Engine

x% = bx%(z%)

DO
  x% = x% + dr%
  IF Map%(x%, by%(z%)) <> 33 THEN EXIT DO
  Map%(x%, by%(z%)) = 0
LOOP

IF Map%(x%, by%(z%)) = 169 THEN
  Map%(x%, by%(z%)) = 420
  mc% = mc% + 1
  IF mc% > 500 THEN mc% = 500
  cant%(mc%) = 1
  canx%(mc%) = x%
  cany%(mc%) = by%(z%)
END IF

Status

ht%(1) = 0: ht%(2) = 0

'=============================================================================


END SUB

SUB BadguyShoot1

IF ps%(1) = 0 THEN dr% = -1 ELSE dr% = 1
x% = px%(1)

DO
  x% = x% + dr%
  IF x% = px%(2) AND py%(1) = py%(2) THEN Life%(2) = Life%(2) - 5: ht%(2) = 1: EXIT DO
  IF x% = bx%(1) AND py%(1) = by%(1) THEN Blife%(1) = Blife%(1) - 5: EXIT DO
  IF Map%(x%, py%(1)) = 48 THEN Map%(x%, py%(1)) = 52
  IF Map%(x%, py%(1)) = 49 THEN Map%(x%, py%(1)) = 53
  IF Map%(x%, py%(1)) = 50 THEN Map%(x%, py%(1)) = 48
  IF Map%(x%, py%(1)) = 51 THEN Map%(x%, py%(1)) = 49
  IF Map%(x%, py%(1)) = 26 THEN Map%(x%, py%(1)) = 28
  IF Map%(x%, py%(1)) = 27 THEN Map%(x%, py%(1)) = 29
  IF Map%(x%, py%(1)) = 23 THEN Map%(x%, py%(1)) = 0
  IF Map%(x%, py%(1)) = 104 THEN Map%(x%, py%(1)) = 596: Alive 1, x%, py%(1)
  IF Map%(x%, py%(1)) = 103 THEN Map%(x%, py%(1)) = 606: Alive 2, x%, py%(1)
  IF Map%(x%, py%(1)) THEN EXIT DO
  Map%(x%, py%(1)) = 33
LOOP

Engine

x% = px%(1)

DO
  x% = x% + dr%
  IF Map%(x%, py%(1)) <> 33 THEN EXIT DO
  Map%(x%, py%(1)) = 0
LOOP

IF Map%(x%, py%(1)) = 169 THEN
  Map%(x%, py%(1)) = 420
  mc% = mc% + 1
  IF mc% > 500 THEN mc% = 500
  cant%(mc%) = 1
  canx%(mc%) = x%
  cany%(mc%) = py%(1)
END IF

Status

ht%(1) = 0: ht%(2) = 0

END SUB

SUB Bazooka (z%)

'***** The bazooka sub
'=============================================================================

  IF BazOn%(z%) = 0 THEN BazOn%(z%) = 1 ELSE BazOn%(z%) = 0
  BazX%(z%) = px%(z%)
  BazY%(z%) = py%(z%)

'=============================================================================

END SUB

SUB BDead (z%)

'***** Make the player dead if his life is below 1 *****
'=============================================================================

  BDeadX%(z%) = bx%(z%): BDeadY%(z%) = by%(z%): BDeadT%(z%) = 1
  AddTimers

'=============================================================================

END SUB

SUB Box (x1%, y1%, x2%, y2%)

LINE (x1% + 1, y1% + 1)-(x2% - 1, y2% - 1), 7, BF
LINE (x1%, y1%)-(x2%, y1%), 15
LINE (x1%, y1%)-(x1%, y2%), 15
LINE (x2%, y1% + 1)-(x2%, y2%), 8
LINE (x1% + 1, y2%)-(x2%, y2%), 8
LINE (x1% + 9, y1% + 9)-(x2% - 9, y1% + 9), 8
LINE (x1% + 9, y1% + 10)-(x1% + 9, y2% - 9), 8
LINE (x1% + 10, y2% - 9)-(x2% - 10, y2% - 9), 15
LINE (x2% - 9, y1% + 10)-(x2% - 9, y2% - 9), 15

END SUB

SUB CheckMap

'***** Check if you hit anything on the map *****
'=============================================================================
FOR z% = 1 TO 2
 
  IF ss% < 1 THEN ss% = 1
  IF ss% > 4 THEN ss% = 4

IF DeadT%(z%) = 0 THEN
 
  IF Map%(opx%(z%), opy%(z%)) = 1 THEN Map%(opx%(z%), opy%(z%)) = 0

  m% = Map%(px%(z%), py%(z%))

  IF m% = 102 THEN BazAmmo%(z%) = BazAmmo%(z%) + 2: m% = -1
 
  IF m% = 678 THEN eh%(z%) = 1: m% = -1
 
  IF m% = 211 THEN Life%(z%) = Life%(z%) + 10: m% = -1
  IF m% = 666 THEN Life%(z%) = Life%(z%) + 25: m% = -1
  IF m% = 672 THEN Life%(z%) = Life%(z%) + 50: m% = -1

  IF m% = 130 THEN Ammo%(z%) = Ammo%(z%) + 25: m% = -1
  IF m% = 132 THEN Ammo%(z%) = Ammo%(z%) + 50: m% = -1
  IF m% = 134 THEN Ammo%(z%) = Ammo%(z%) + 100: m% = -1

  IF m% = 30 THEN Mines%(z%) = Mines%(z%) + 5: m% = -1
  IF Sweeper%(z%) = 0 AND m% = 64 THEN Sweeper%(z%) = 1: m% = -1
  IF m% = 54 THEN Life%(z%) = Life%(z%) - 10: ht%(z%) = 1
  IF m% = 55 THEN Life%(z%) = Life%(z%) - 10: ht%(z%) = 1: Poison%(z%) = 1
  IF m% = 61 THEN Life%(z%) = Life%(z%) - 15: ht%(z%) = 1: m% = -1
  IF m% = 62 THEN Life%(z%) = Life%(z%) - 15: ht%(z%) = 1: m% = -1
  IF m% = 63 THEN Life%(z%) = Life%(z%) - 15: ht%(z%) = 1: m% = -1
  IF m% = 684 AND Poison%(z%) = 1 THEN Poison%(z%) = 0: m% = -1
  IF m% > 56 AND m% < 61 THEN Life%(z%) = Life%(z%) - 5: ht%(z%) = 1

  IF m% > 105 AND m% < 117 THEN
    FOR n% = 1 TO kl%(z%)
      IF m% = KeyCard%(n%, z%) THEN dg% = 1
    NEXT n%
    IF dg% = 0 THEN
      kl%(z%) = kl%(z%) + 1
      KeyCard%(kl%(z%), z%) = m%
      m% = -1
    END IF
    dg% = 0
  END IF
 
  IF m% > 117 AND m% < 129 THEN
    FOR n% = 1 TO kl%(z%)
      IF KeyCard%(n%, z%) = m% - 12 THEN m% = -1: EXIT FOR
    NEXT n%
  END IF

  IF Map%(opx%(z%), opy%(z%)) = 0 AND m% > 64 AND m% < 68 THEN SWAP Weapon%(z%), Map%(px%(z%), py%(z%)): m% = -2

  IF m% = 136 OR m% = 138 THEN
  FOR n% = 1 TO 2
    IF px%(z%) = tx1%(n%) AND py%(z%) = ty1%(n%) THEN
      BMap%(px%(2), py%(2), 3) = -1
      FOR y% = -1 TO 1
      FOR x% = -1 TO 1
        IF n% = 1 THEN IF Map%(tx1%(2) + x%, ty1%(2) + y%) = 0 THEN px%(z%) = tx1%(2) + x%: py%(z%) = ty1%(2) + y%: m% = -1
        IF n% = 2 THEN IF Map%(tx1%(1) + x%, ty1%(1) + y%) = 0 THEN px%(z%) = tx1%(1) + x%: py%(z%) = ty1%(1) + y%: m% = -1
      NEXT x%
      NEXT y%
      IF n% = 1 THEN BMap%(tx1%(2), ty1%(2), 3) = 0
      IF n% = 2 THEN BMap%(tx1%(1), ty1%(1), 3) = 0
    END IF
    IF px%(z%) = tx2%(n%) AND py%(z%) = ty2%(n%) THEN
      BMap%(px%(2), py%(2), 3) = -1
      FOR y% = -1 TO 1
      FOR x% = -1 TO 1
        IF n% = 1 THEN IF Map%(tx2%(2) + x%, ty2%(2) + y%) = 0 THEN px%(z%) = tx2%(2) + x%: py%(z%) = ty2%(2) + y%: m% = -1
        IF n% = 2 THEN IF Map%(tx2%(1) + x%, ty2%(1) + y%) = 0 THEN px%(z%) = tx2%(1) + x%: py%(z%) = ty2%(1) + y%: m% = -1
      NEXT x%
      NEXT y%
    IF n% = 1 THEN BMap%(tx2%(2), ty2%(2), 3) = 0
    IF n% = 2 THEN BMap%(tx2%(1), ty2%(1), 3) = 0
    END IF
  NEXT n%
  END IF

  IF m% = 227 THEN ExtraAmmo%(z%) = 150: m% = -1

  IF m% = 68 AND Baz%(z%) = 0 THEN Baz%(z%) = 1: BazAmmo%(z%) = BazAmmo%(z%) + 2: m% = -1
  IF BazOn%(z%) = 1 THEN m% = -2

  IF m% <> -1 AND m% <> -2 THEN IF Map%(px%(z%), py%(z%)) THEN px%(z%) = opx%(z%): py%(z%) = opy%(z%)
 
  IF Map%(px%(z%), py%(z%)) = 0 THEN m% = -1
  IF m% = -1 THEN Map%(px%(z%), py%(z%)) = 1: Status
 
  IF BazX%(z%) < 2 THEN BazX%(z%) = 2
  IF BazX%(z%) > 49 THEN BazX%(z%) = 49
  IF BazY%(z%) < 2 THEN BazY%(z%) = 2
  IF BazY%(z%) > 49 THEN BazY%(z%) = 49

END IF

NEXT z%
'=============================================================================

END SUB

SUB Credits

SCREEN 0
CLS
WIDTH 80, 25

PRINT "Toadman 2:  Space Shock  - A Delta Code production"
PRINT "--------------------------------------------------------------------------------"
PRINT "Programming :  Joe King"
PRINT "Graphics    :  Joe King"
PRINT "Level Design:  Joe King"
PRINT "Testers     :  Joe King"
PRINT "               James Heath"
PRINT "               Matthew R. Knight"
PRINT "               Daniel King"
PRINT "               Robert Wysher"
PRINT ""
PRINT "Special Thanks:  Frogman, Tim Truman's Sprite Editor"
PRINT "--------------------------------------------------------------------------------"
PRINT "AntiCopyright(AC) July, 2000"
PRINT "The following people are NOT members of Delta Code:  Matthew R. Knight, Daniel"
PRINT "King, Robert Wysher, and Tim Truman"
PRINT "--------------------------------------------------------------------------------"
LOCATE , 31
PRINT "Thanks for playing"
COLOR 14
LOCATE , 35
PRINT "TOADMAN 2"
COLOR 10
LOCATE , 34
PRINT "space shock"
SLEEP
SCREEN 13
DIM Pal(255) AS HUES
DEF SEG = VARSEG(Pal(0))
BLOAD "default.PAL", 0
OUT &H3C8, 0
FOR atrib = 0 TO 255
 OUT &H3C9, Pal(atrib).Red
 OUT &H3C9, Pal(atrib).grn
 OUT &H3C9, Pal(atrib).blu
NEXT atrib
END SUB

SUB Dead (z%)

'***** Make the player dead if his life is below 1 *****
'=============================================================================

  DeadX%(z%) = px%(z%): DeadY%(z%) = py%(z%): DeadT%(z%) = 1: BazOn%(z%) = 0

'=============================================================================

END SUB

SUB DropMine (z%)

'Drop a mine

IF ps%(z%) = 0 AND Map%(px%(z%) - 1, py%(z%)) = 0 AND Mines%(z%) > 0 THEN
Map%(px%(z%) - 1, py%(z%)) = 61 + z%
Mines%(z%) = Mines%(z%) - 1
END IF

IF ps%(z%) = 2 AND Map%(px%(z%) + 1, py%(z%)) = 0 AND Mines%(z%) > 0 THEN
  Map%(px%(z%) + 1, py%(z%)) = 61 + z%
  Mines%(z%) = Mines%(z%) - 1
END IF

END SUB

SUB Engine

'***** The 2d tile*tile engine *****
'=============================================================================

  Count1& = Count1& + 1
  Count2& = Count2& + 1
  IF Count1& > ASpeed& THEN Count1& = 0: AddTimers
  IF Count2& > BSpeed& THEN Count2& = 0: MoveBadguys
FOR z% = 1 TO 2

IF BazOn%(z%) = 0 THEN xx% = px%(z%): yy% = py%(z%)
IF BazOn%(z%) = 1 THEN xx% = BazX%(z%): yy% = BazY%(z%)

FOR y% = -ss% TO ss%
FOR x% = -ss% TO ss%

  sn% = -1

  IF xx% + x% < 1 THEN sn% = OuterBorder%
  IF yy% + y% < 1 THEN sn% = OuterBorder%
  IF xx% + x% > 50 THEN sn% = OuterBorder%
  IF yy% + y% > 50 THEN sn% = OuterBorder%
 
  IF sn% <> OuterBorder% THEN sn% = Map%(xx% + x%, yy% + y%)
 
  IF sn% > 105 AND sn% < 175 THEN sn% = sn% + tm%(2)
  IF sn% > 175 AND sn% < 195 THEN sn% = sn% + tm%(3)
  IF sn% > 210 AND sn% < 245 THEN sn% = sn% + tm%(4)
  IF sn% > 665 AND sn% < 700 THEN sn% = sn% + tm%(5)
  IF sn% > 194 AND sn% < 210 THEN sn% = sn% + tm%(7)

   
  '***** Check for canaster *****
    IF sn% = 420 THEN
      FOR n% = 1 TO mc%
        IF xx% + x% = canx%(n%) AND yy% + y% = cany%(n%) THEN sn% = sn% + cant%(n%): EXIT FOR
      NEXT n%
    END IF

  '***** Check for frozen toad *****
  FOR n% = 1 TO 2
    IF BDeadT%(n%) = 0 THEN
      IF n% = 2 THEN p% = 4 ELSE p% = 0
      IF al%(n%) = 2 THEN IF xx% + x% = bx%(n%) AND yy% + y% = by%(n%) THEN sn% = (36 + bps%(n%)) + tm%(2) + p%
    END IF
  NEXT n%

  IF BazOn%(1) = 0 THEN
    IF z% = 1 AND px%(1) + x% = px%(2) AND py%(1) + y% = py%(2) THEN
      IF DeadT%(2) < 1 AND ht%(2) = 0 THEN sn% = 5 + tm%(2) + ps%(2) ELSE sn% = 11 + (ps%(2) / 2)
    END IF
  END IF

  IF BazOn%(1) = 1 AND z% = 1 THEN
    IF BazX%(1) + x% = px%(1) AND BazY%(1) + y% = py%(1) THEN
      IF DeadT%(1) < 1 AND ht%(1) = 0 THEN sn% = 1 + tm%(2) + ps%(1) ELSE sn% = 9 + (ps%(1) / 2)
    END IF
    IF BazX%(1) + x% = px%(2) AND BazY%(1) + y% = py%(2) THEN
      IF DeadT%(2) < 1 AND ht%(2) = 0 THEN sn% = 5 + tm%(2) + ps%(2) ELSE sn% = 11 + (ps%(2) / 2)
    END IF
  END IF
 
  IF BazOn%(2) = 0 THEN
    IF z% = 2 AND px%(2) + x% = px%(1) AND py%(2) + y% = py%(1) THEN
      IF DeadT%(1) < 1 AND ht%(1) = 0 THEN sn% = 1 + tm%(2) + ps%(1) ELSE sn% = 9 + (ps%(1) / 2)
    END IF
  END IF

  IF BazOn%(2) = 1 AND z% = 2 THEN
    IF BazX%(2) + x% = px%(1) AND BazY%(2) + y% = py%(1) THEN
      IF DeadT%(1) = 0 AND ht%(1) = 0 THEN sn% = 1 + tm%(2) + ps%(1) ELSE sn% = 9 + (ps%(1) / 2)
    END IF
    IF BazX%(2) + x% = px%(2) AND BazY%(2) + y% = py%(2) THEN
      IF DeadT%(2) = 0 AND ht%(2) = 0 THEN sn% = 5 + tm%(2) + ps%(2) ELSE sn% = 11 + (ps%(2) / 2)
    END IF
  END IF

    IF xx% + x% = px%(1) AND yy% + y% = py%(1) AND DeadT%(1) > 0 THEN sn% = Map%(xx% + x%, yy% + y%)
    IF xx% + x% = px%(2) AND yy% + y% = py%(2) AND DeadT%(2) > 0 THEN sn% = Map%(xx% + x%, yy% + y%)
    IF z% = 2 THEN IF xx% + x% = px%(1) AND yy% + y% = py%(1) AND DeadT%(1) = -1 THEN sn% = Map%(xx% + x%, yy% + y%)
    IF z% = 1 THEN IF xx% + x% = px%(2) AND yy% + y% = py%(2) AND DeadT%(2) = -1 THEN sn% = Map%(xx% + x%, yy% + y%)
    IF z% = 2 AND sn% = 62 AND Sweeper%(2) = 0 THEN sn% = 0
    IF z% = 1 AND sn% = 63 AND Sweeper%(1) = 0 THEN sn% = 0
    IF z% = 2 AND sn% = 61 AND Sweeper%(2) = 0 THEN sn% = 0
    IF z% = 1 AND sn% = 61 AND Sweeper%(1) = 0 THEN sn% = 0

  IF z% = 1 THEN p% = 0 ELSE p% = 126
  IF x% <> 0 OR y% <> 0 THEN PUT (x% * 8 - 8 + 104 + p%, y% * 8 - 8 + 80), sprites%(eps% * sn%), PSET
  IF DeadT%(z%) > 0 AND x% = 0 AND y% = 0 THEN PUT (x% * 8 - 8 + 104 + p%, y% * 8 - 8 + 80), sprites%(eps% * sn%), PSET
  IF DeadT%(z%) = -1 AND x% = 0 AND y% = 0 THEN PUT (x% * 8 - 8 + 104 + p%, y% * 8 - 8 + 80), sprites%(eps% * sn%), PSET

  IF sn% <> OuterBorder% THEN PSET (px%(z%) + x%, py%(z%) + y%), POINT(x% * 8 - 8 + 104 + p% + 3, y% * 8 - 8 + 84)
  IF z% = 1 THEN PSET (px%(1), py%(1)), 4
  IF z% = 2 THEN PSET (px%(2), py%(2)), 1
 
NEXT x%
NEXT y%
  IF Life%(z%) < 1 AND eh%(z%) = 1 THEN Life%(z%) = 100: eh%(z%) = 0: Status
  IF Life%(z%) < 1 AND DeadT%(z%) = 0 THEN Dead z%
  IF Blife%(z%) < 0 AND BDeadT%(z%) = 0 THEN BDead z%
NEXT z%

  IF Blife%(1) > 0 THEN PSET (bx%(1), by%(1)), 14
  IF Blife%(2) > 0 THEN PSET (bx%(2), by%(2)), 2
 
  IF ht%(1) = 0 THEN sn1% = 1 + tm%(2) + ps%(1) ELSE sn1% = 9 + (ps%(1) / 2)
  IF ht%(2) = 0 THEN sn2% = 5 + tm%(2) + ps%(2) ELSE sn2% = 11 + (ps%(2) / 2)
  IF BazOn%(1) = 1 THEN sn1% = 69
  IF BazOn%(2) = 1 THEN sn2% = 69

  IF DeadT%(1) = 0 THEN PUT (96, 72), sprites%(eps% * sn1%), PSET
  IF DeadT%(2) = 0 THEN PUT (222, 72), sprites%(eps% * sn2%), PSET

'=============================================================================

END SUB

SUB Fiddle (z%)

'***** I'm not telling you what this does, it's SECRET *****
'=============================================================================
      KeyCard%(1, z%) = 106
      KeyCard%(2, z%) = 108
      KeyCard%(3, z%) = 110
      KeyCard%(4, z%) = 112
      KeyCard%(5, z%) = 114
      KeyCard%(6, z%) = 116
      kl%(z%) = 6
      Weapon%(z%) = 67
      Mines%(z%) = 99
      Sweeper%(z%) = 1
      Baz%(z%) = 1
      BazAmmo%(z%) = 99
      Status
'=============================================================================

END SUB

SUB Hit (z%)

'***** Check around you to see if you hit something *****
'=============================================================================

  FOR y% = -1 TO 1
  FOR x% = -1 TO 1
    m% = Map%(px%(z%) + x%, py%(z%) + y%)
    h% = 35 * (Weapon%(z%) - 64)
    IF h% < 1 THEN h% = 0
    IF h% = 3 THEN h% = 5
    IF h% = 2 THEN h% = 3
   
    IF m% > 245 AND m% < 420 THEN
      Map%(px%(z%) + x%, py%(z%) + y%) = Map%(px%(z%) + x%, py%(z%) + y%) + h%
      IF Map%(px%(z%) + x%, py%(z%) + y%) > 420 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 0
    END IF
 
    IF m% > 140 AND m% < 147 THEN Map%(px%(z%) + x%, py%(z%) + y%) = Map%(px%(z%) + x%, py%(z%) + y%) + 6
    IF m% > 175 AND m% < 185 THEN Map%(px%(z%) + x%, py%(z%) + y%) = Map%(px%(z%) + x%, py%(z%) + y%) + 9
   
    IF m% = 23 AND h% > 0 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 0

    IF z% = 1 THEN IF px%(1) + x% = px%(2) AND py%(1) + y% = py%(2) THEN Life%(2) = Life%(2) - (h% / 35): Status
    IF z% = 2 THEN IF px%(2) + x% = px%(1) AND py%(2) + y% = py%(1) THEN Life%(1) = Life%(1) - (h% / 35): Status
   
    FOR n% = 1 TO 2
      IF px%(n%) + x% = bx%(2) AND py%(n%) + y% = by%(2) THEN Blife%(2) = Blife%(2) - (h% / 35)
      IF px%(n%) + x% = bx%(1) AND py%(n%) + y% = by%(1) THEN Blife%(1) = Blife%(1) - (h% / 35)
    NEXT n%

    IF h% > 0 THEN
      IF m% = 48 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 52
      IF m% = 49 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 53
      IF m% = 50 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 48
      IF m% = 51 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 49
      IF m% = 26 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 28
      IF m% = 27 THEN Map%(px%(z%) + x%, py%(z%) + y%) = 29
    END IF
   
    IF m% = 169 AND h% > 0 THEN
      Map%(px%(z%) + x%, py%(z%) + y%) = 420
      mc% = mc% + 1
      IF mc% > 500 THEN mc% = 500
      cant%(mc%) = 1
      canx%(mc%) = px%(z%) + x%
      cany%(mc%) = py%(z%) + y%
    END IF

  NEXT x%
  NEXT y%

  IF Life%(1) < 0 THEN Life%(1) = 0
  IF Life%(2) < 0 THEN Life%(2) = 0
 
'=============================================================================

END SUB

SUB Intro

'***** Put up the intro screen *****
'=============================================================================

  SLEEP (1)
CLS
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (16 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 631), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (17 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 632), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (18 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 633), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (19 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 634), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (20 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 635), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (21 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 636), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (22 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 637), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
PUT (24 * 8 - 8, 10 * 8 - 8), sprites%(eps% * 638), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&

FOR n% = 1 TO 40
  PUT (n% * 8 - 8, 12 * 8 - 8), sprites%(eps% * 33), PSET
  FOR Delay& = -IntroSpeed& / 2 TO IntroSpeed& / 2: NEXT Delay&
NEXT n%

PUT (17 * 8 - 8, 11 * 8 - 8), sprites%(eps% * 639), PSET
PUT (18 * 8 - 8, 11 * 8 - 8), sprites%(eps% * 640), PSET
PUT (19 * 8 - 8, 11 * 8 - 8), sprites%(eps% * 641), PSET
PUT (21 * 8 - 8, 11 * 8 - 8), sprites%(eps% * 642), PSET
PUT (22 * 8 - 8, 11 * 8 - 8), sprites%(eps% * 643), PSET
PUT (23 * 8 - 8, 11 * 8 - 8), sprites%(eps% * 644), PSET

LOCATE 1, 15
COLOR 15
PRINT "Version 1.0"
LOCATE 15, 14
COLOR 14
PRINT "Press any key"

DO
  a$ = INKEY$
  PUT (14 * 8 - 8, 10 * 8 - 8), sprites%(eps% * (456 + an%)), PSET
  PUT (26 * 8 - 8, 10 * 8 - 8), sprites%(eps% * (491 + an%)), PSET
  FOR Delay& = -IntroSpeed& TO IntroSpeed&: NEXT Delay&
  an% = an% + 1
  IF an% > 27 THEN SLEEP (1): an% = 0
LOOP UNTIL a$ <> ""

MainList:

CLS

LOCATE 1, 1
COLOR 15
PRINT "Choose>>>"
COLOR 2
PRINT "         Docking Bay"
PRINT "         Research Center"
PRINT "         The Core"
PRINT "         Hallways"
PRINT "         Orbit"
PRINT "         Mine Field"
PRINT "         Winter"
PRINT "         Magma Caves"
PRINT "         The Arena"
PRINT "         Toad Bar"
PRINT "         The Museum"
COLOR 3
PRINT "         Credits"
PRINT "         Random Map"
PRINT "         Load Custom Map"

x% = 2
COLOR 10

DO
  LOCATE x%, 1
  COLOR 14
  IF x% = 2 THEN PRINT "  "; : COLOR 10: PRINT "      Docking Bay"
  IF x% = 3 THEN PRINT "  "; : COLOR 10: PRINT "      Research Center"
  IF x% = 4 THEN PRINT "  "; : COLOR 10: PRINT "      The Core"
  IF x% = 5 THEN PRINT "  "; : COLOR 10: PRINT "      Hallways"
  IF x% = 6 THEN PRINT "  "; : COLOR 10: PRINT "      Orbit"
  IF x% = 7 THEN PRINT "  "; : COLOR 10: PRINT "      Mine Field"
  IF x% = 8 THEN PRINT "  "; : COLOR 10: PRINT "      Winter"
  IF x% = 9 THEN PRINT "  "; : COLOR 10: PRINT "      Magma Caves"
  IF x% = 10 THEN PRINT "  "; : COLOR 10: PRINT "      The Arena"
  IF x% = 11 THEN PRINT "  "; : COLOR 10: PRINT "      Toad Bar"
  IF x% = 12 THEN PRINT "  "; : COLOR 10: PRINT "      The Museum"
  IF x% = 13 THEN PRINT "  "; : COLOR 11: PRINT "      Credits"
  IF x% = 14 THEN PRINT "  "; : COLOR 11: PRINT "      Random Map"
  IF x% = 15 THEN PRINT "  "; : COLOR 11: PRINT "      Load Custom Map"
  DO
    a$ = INKEY$
  LOOP UNTIL a$ <> ""
    LOCATE x%, 1
    IF x% = 2 THEN COLOR 2: PRINT "         Docking Bay"
    IF x% = 3 THEN COLOR 2: PRINT "         Research Center"
    IF x% = 4 THEN COLOR 2: PRINT "         The Core"
    IF x% = 5 THEN COLOR 2: PRINT "         Hallways"
    IF x% = 6 THEN COLOR 2: PRINT "         Orbit"
    IF x% = 7 THEN COLOR 2: PRINT "         Mine Field"
    IF x% = 8 THEN COLOR 2: PRINT "         Winter"
    IF x% = 9 THEN COLOR 2: PRINT "         Magma Caves"
    IF x% = 10 THEN COLOR 2: PRINT "         The Arena"
    IF x% = 11 THEN COLOR 2: PRINT "         Toad Bar"
    IF x% = 12 THEN COLOR 2: PRINT "         The Museum"
    IF x% = 13 THEN COLOR 3: PRINT "         Credits"
    IF x% = 14 THEN COLOR 3: PRINT "         Random Map"
    IF x% = 15 THEN COLOR 3: PRINT "         Load Custom Map"
    IF a$ = CHR$(0) + "P" THEN x% = x% + 1
    IF a$ = CHR$(0) + "H" THEN x% = x% - 1
    IF a$ = CHR$(27) THEN END
    IF a$ = CHR$(13) THEN EXIT DO
    IF x% > 15 THEN x% = 2
    IF x% < 2 THEN x% = 15
  LOOP

  CLS

LoadMaps:

  ld% = x%
  IF ld% = 2 THEN load "Dock"
  IF ld% = 3 THEN load "Research"
  IF ld% = 4 THEN load "core"
  IF ld% = 5 THEN load "hallways"
  IF ld% = 6 THEN load "Orbit"
  IF ld% = 7 THEN load "mine"
  IF ld% = 8 THEN load "winter"
  IF ld% = 9 THEN load "magma"
  IF ld% = 10 THEN load "arena"
  IF ld% = 11 THEN load "ToadBar"
  IF ld% = 12 THEN load "Museum"
  IF ld% = 13 THEN Credits: SCREEN 13: GOTO MainList
  IF ld% = 14 THEN x% = INT(10 * RND(1)) + 2: GOTO LoadMaps
  IF ld% = 15 THEN
    CLS
    COLOR 15
    INPUT "Enter Name/", nm$
    load nm$
    CLS
  END IF

'=============================================================================

END SUB

SUB LeaveTrail (z%)

'***** Leaves a trail for the frozen toads to follow *****
'=============================================================================

  c% = INT(8 * RND(1)) + 1
  IF z% <> 3 THEN IF c% <> 1 THEN BMap%(opx%(z%), opy%(z%), z%) = -1
  IF z% = 3 THEN IF c% <> 1 THEN BMap%(opx%(2), opy%(2), z%) = -1

'=============================================================================

END SUB

SUB load (nm$)

t1% = 1: t2% = 1

  DEF SEG = VARSEG(Map%(0, 0))
  BLOAD nm$ + ".td2", VARPTR(Map%(0, 0))
  OuterBorder% = Map%(0, 0)
FOR y% = 1 TO 50
FOR x% = 1 TO 50
  IF Map%(x%, y%) = 1 THEN px%(1) = x%: py%(1) = y%: Map%(x%, y%) = 0
  IF Map%(x%, y%) = 5 THEN px%(2) = x%: py%(2) = y%: Map%(x%, y%) = 0
  IF Map%(x%, y%) = 136 THEN tx1%(t1%) = x%: ty1%(t1%) = y%: t1% = t1% + 1
  IF Map%(x%, y%) = 138 THEN tx2%(t2%) = x%: ty2%(t2%) = y%: t2% = t2% + 1
NEXT x%
NEXT y%

END SUB

SUB MoveBadguys

'***** Move the frozen toads *****
'***** And the red-eyed toad if OnePlayerMode is enabled *****
'=============================================================================

FOR z% = 1 TO 3

IF z% = 3 AND OnePlayerMode% = 0 THEN EXIT FOR

IF z% = 3 AND Life%(1) > 0 THEN
 
    opx%(1) = px%(1): opy%(1) = py%(1)
    lr% = 0: ud% = 0

    FOR y% = -1 TO 1
    FOR x% = -1 TO 1
      IF px%(1) + x% = px%(2) AND py%(1) + y% = py%(2) THEN Life%(2) = Life%(2) - 2: ht%(2) = 1: Status
      IF px%(1) + x% = bx%(1) AND py%(1) + y% = by%(1) THEN Blife%(2) = Blife%(2) - 2: Status
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(px%(1) + x%, py%(1) + y%) = 211 THEN px%(1) = px%(1) + x%: py%(1) = py%(1) + y%: Life%(1) = Life%(1) + 10: Map%(px%(1), py%(1)) = 0: sk% = 1
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(px%(1) + x%, py%(1) + y%) = 666 THEN px%(1) = px%(1) + x%: py%(1) = py%(1) + y%: Life%(1) = Life%(1) + 25: Map%(px%(1), py%(1)) = 0: sk% = 1
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(px%(1) + x%, py%(1) + y%) = 672 THEN px%(1) = px%(1) + x%: py%(1) = py%(1) + y%: Life%(1) = Life%(1) + 50: Map%(px%(1), py%(1)) = 0: sk% = 1
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(px%(1) + x%, py%(1) + y%) = 136 AND BMap%(px%(1) + x%, py%(1) + y%, 3) = -1 THEN px%(1) = px%(1) + x%: py%(1) = py%(1) + y%: CheckMap: sk% = 1
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(px%(1) + x%, py%(1) + y%) = 138 AND BMap%(px%(1) + x%, py%(1) + y%, 3) = -1 THEN px%(1) = px%(1) + x%: py%(1) = py%(1) + y%: CheckMap: sk% = 1
    NEXT x%
    NEXT y%
 
    IF sk% = 1 THEN sk% = 0: GOTO SkipMove1

    mn% = 32000
  
    FOR y% = -1 TO 1
    FOR x% = -1 TO 1
      IF x% = 0 OR y% = 0 THEN
      bm% = BMap%(px%(1) + x%, py%(1) + y%, 3)
      mp% = Map%(px%(1) + x%, py%(1) + y%)
      IF bm% < mn% AND mp% = 0 THEN mn% = bm%: lr% = x%: ud% = y%
      END IF
    NEXT x%
    NEXT y%
   
      IF px%(1) < px%(2) AND BMap%(px%(1) + 1, py%(1), 3) = mn% AND Map%(px%(1) + 1, py%(1)) = 0 THEN lr% = 1: ud% = 0
      IF px%(1) > px%(2) AND BMap%(px%(1) - 1, py%(1), 3) = mn% AND Map%(px%(1) - 1, py%(1)) = 0 THEN lr% = -1: ud% = 0
      IF py%(1) < py%(2) AND BMap%(px%(1), py%(1) + 1, 3) = mn% AND Map%(px%(1), py%(1) + 1) = 0 THEN lr% = 0: ud% = 1
      IF py%(1) > py%(2) AND BMap%(px%(1), py%(1) - 1, 3) = mn% AND Map%(px%(1), py%(1) - 1) = 0 THEN lr% = 0: ud% = -1
    
    BMap%(px%(1), py%(1), 3) = BMap%(px%(1), py%(1), 3) + 1
    IF lr% = 1 THEN ps%(1) = 2
    IF lr% = -1 THEN ps%(1) = 0
    px%(1) = px%(1) + lr%
    py%(1) = py%(1) + ud%
 
SkipMove1:
  
    IF Map%(opx%(1), opy%(1)) = 1 THEN Map%(opx%(1), opy%(1)) = 0
   
    IF py%(1) = py%(2) THEN
      IF px%(1) < px%(2) THEN ps%(1) = 2 ELSE ps%(1) = 0
      BadguyShoot1
    END IF
    IF py%(1) = by%(1) THEN
      IF px%(1) < bx%(1) THEN ps%(1) = 2 ELSE ps%(1) = 0
      BadguyShoot1
    END IF

END IF

IF z% = 3 AND OnePlayerMode% = 1 THEN EXIT FOR

IF BDeadT%(z%) = 0 THEN
 
  IF al%(z%) = 2 THEN
   
    obx%(z%) = bx%(z%): oby%(z%) = by%(z%)
    lr% = 0: ud% = 0

    FOR y% = -1 TO 1
    FOR x% = -1 TO 1
      IF bx%(z%) + x% = px%(1) AND by%(z%) + y% = py%(1) THEN Life%(1) = Life%(1) - 2: ht%(1) = 1: Status
      IF bx%(z%) + x% = px%(2) AND by%(z%) + y% = py%(2) THEN Life%(2) = Life%(2) - 2: ht%(2) = 1: Status
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(bx%(z%) + x%, by%(z%) + y%) = 211 THEN bx%(z%) = bx%(z%) + x%: by%(z%) = by%(z%) + y%: Blife%(z%) = Blife%(z%) + 10: Map%(bx%(z%), by%(z%)) = 0: sk% = 1
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(bx%(z%) + x%, by%(z%) + y%) = 666 THEN bx%(z%) = bx%(z%) + x%: by%(z%) = by%(z%) + y%: Blife%(z%) = Blife%(z%) + 25: Map%(bx%(z%), by%(z%)) = 0: sk% = 1
      IF sk% = 0 THEN IF x% = 0 OR y% = 0 THEN IF Map%(bx%(z%) + x%, by%(z%) + y%) = 672 THEN bx%(z%) = bx%(z%) + x%: by%(z%) = by%(z%) + y%: Blife%(z%) = Blife%(z%) + 50: Map%(bx%(z%), by%(z%)) = 0: sk% = 1
    NEXT x%
    NEXT y%
  
    IF sk% = 1 THEN sk% = 0: GOTO SkipMove

    mn% = 32000
   
    FOR y% = -1 TO 1
    FOR x% = -1 TO 1
      IF x% = 0 OR y% = 0 THEN
      bm% = BMap%(bx%(z%) + x%, by%(z%) + y%, z%)
      mp% = Map%(bx%(z%) + x%, by%(z%) + y%)
      IF bm% < mn% AND mp% = 0 THEN mn% = bm%: lr% = x%: ud% = y%
      END IF
    NEXT x%
    NEXT y%
    
      IF bx%(z%) < px%(z%) AND BMap%(bx%(z%) + 1, by%(z%), z%) = mn% AND Map%(bx%(z%) + 1, by%(z%)) = 0 THEN lr% = 1: ud% = 0
      IF bx%(z%) > px%(z%) AND BMap%(bx%(z%) - 1, by%(z%), z%) = mn% AND Map%(bx%(z%) - 1, by%(z%)) = 0 THEN lr% = -1: ud% = 0
      IF by%(z%) < py%(z%) AND BMap%(bx%(z%), by%(z%) + 1, z%) = mn% AND Map%(bx%(z%), by%(z%) + 1) = 0 THEN lr% = 0: ud% = 1
      IF by%(z%) > py%(z%) AND BMap%(bx%(z%), by%(z%) - 1, z%) = mn% AND Map%(bx%(z%), by%(z%) - 1) = 0 THEN lr% = 0: ud% = -1
     
    BMap%(bx%(z%), by%(z%), z%) = BMap%(bx%(z%), by%(z%), z%) + 1
    IF lr% = 1 THEN bps%(z%) = 2
    IF lr% = -1 THEN bps%(z%) = 0
    bx%(z%) = bx%(z%) + lr%
    by%(z%) = by%(z%) + ud%
  
SkipMove:
   
    IF Map%(obx%(z%), oby%(z%)) = 1 THEN Map%(obx%(z%), oby%(z%)) = 0
    Map%(bx%(z%), by%(z%)) = 1

    IF by%(z%) = py%(z%) THEN
      IF bx%(z%) < px%(z%) THEN bps%(z%) = 2 ELSE bps%(z%) = 0
      BadguyShoot z%
    END IF

  END IF

END IF

  IF bx%(z%) <> 0 AND by%(z%) <> 0 THEN PSET (obx%(z%), oby%(z%)), 0
  IF z% = 1 AND Blife%(1) > 0 THEN PSET (bx%(1), by%(1)), 14
  IF z% = 2 AND Blife%(2) > 0 THEN PSET (bx%(2), by%(2)), 2

NEXT z%

'=============================================================================

END SUB

SUB Restart

'***** Restart the level *****
'=============================================================================

  CLS
 
  FOR z% = 1 TO 2
    Life%(z%) = 100
    Ammo%(z%) = 150
    BazOn%(z%) = 0
    BazAmmo%(z%) = 0
    Baz%(z%) = 0
    Weapon%(z%) = 0
    KeyCard%(1, z%) = 0
    KeyCard%(2, z%) = 0
    KeyCard%(3, z%) = 0
    KeyCard%(4, z%) = 0
    KeyCard%(5, z%) = 0
    KeyCard%(6, z%) = 0
    kl%(z%) = 0
    ps%(z%) = 0
    al%(z%) = 0
    eh%(z%) = 0
    ExtraAmmo%(z%) = 0
    DeadT%(z%) = 0
    ht%(z%) = 0
    Poison%(z%) = 0
    Sweeper%(z%) = 0
    ax%(z%) = 0
    ay%(z%) = 0
    al%(z%) = 0
    at%(z%) = 0
    FOR y% = 1 TO 50
    FOR x% = 1 TO 50
      BMap%(x%, y%, z%) = 0
    NEXT x%
    NEXT y%
    BDeadT%(z%) = 0
    bx%(z%) = 0: by%(z%) = 0
  NEXT z%
 
  al%(3) = 0
  FOR y% = 1 TO 50
  FOR x% = 1 TO 50
    BMap%(x%, y%, 3) = 0
  NEXT x%
  NEXT y%
 
  IF ld% = 2 THEN load "Dock"
  IF ld% = 3 THEN load "Research"
  IF ld% = 4 THEN load "core"
  IF ld% = 5 THEN load "hallways"
  IF ld% = 6 THEN load "Orbit"
  IF ld% = 7 THEN load "mine"
  IF ld% = 8 THEN load "winter"
  IF ld% = 9 THEN load "magma"
  IF ld% = 10 THEN load "arena"
  IF ld% = 11 THEN load "ToadBar"
  IF ld% = 12 THEN load "Museum"

Box 54, 30, 145, 121
Box 180, 30, 271, 121

'=============================================================================

END SUB

SUB Shoot (z%)

'***** The shooting sub *****
'=============================================================================

IF tm%(6) = 0 AND BazOn%(z%) = 1 AND BazAmmo%(z%) > 0 THEN
  FOR n% = 1 TO 2
  FOR y% = -1 TO 1
  FOR x% = -1 TO 1
    IF BazX%(z%) + x% = bx%(1) AND BazY%(z%) + y% = by%(1) THEN Blife%(1) = Blife%(1) - 30
    IF BazX%(z%) + x% = bx%(2) AND BazY%(z%) + y% = by%(2) THEN Blife%(2) = Blife%(2) - 30
    IF x% = 0 AND y% = 0 AND BazX%(z%) + x% = bx%(1) AND BazY%(z%) + y% = by%(1) THEN Blife% = Blife% - 20
    IF x% = 0 AND y% = 0 AND BazX%(z%) + x% = bx%(2) AND BazY%(z%) + y% = by%(2) THEN Blife% = Blife% - 20
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 104 THEN Alive 1, BazX%(z%) + x%, BazY%(z%) + y%
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 103 THEN Alive 2, BazX%(z%) + x%, BazY%(z%) + y%
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 48 THEN Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 52
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 49 THEN Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 53
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 50 THEN Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 48
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 51 THEN Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 49
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 26 THEN Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 28
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 27 THEN Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 29
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) > 140 AND Map%(BazX%(z%) + x%, BazY%(z%) + y%) < 147 THEN
      Map%(BazX%(z%) + x%, BazY%(z%) + y%) = Map%(BazX%(z%) + x%, BazY%(z%) + y%) + 6
    END IF
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) > 175 AND Map%(BazX%(z%) + x%, BazY%(z%) + y%) < 185 THEN
      Map%(BazX%(z%) + x%, BazY%(z%) + y%) = Map%(BazX%(z%) + x%, BazY%(z%) + y%) + 9
    END IF
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 0 OR Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 23 THEN
      Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 420
      mc% = mc% + 1
      IF mc% > 500 THEN mc% = 500
      cant%(mc%) = 3
      canx%(mc%) = BazX%(z%) + x%
      cany%(mc%) = BazY%(z%) + y%
    END IF
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 169 THEN
      Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 420
      mc% = mc% + 1
      IF mc% > 500 THEN mc% = 500
      cant%(mc%) = 1
      canx%(mc%) = BazX%(z%) + x%
      cany%(mc%) = BazY%(z%) + y%
    END IF
    IF Map%(BazX%(z%) + x%, BazY%(z%) + y%) >= 245 AND Map%(BazX%(z%) + x%, BazY%(z%) + y%) < 420 THEN
      Map%(BazX%(z%) + x%, BazY%(z%) + y%) = 420
      mc% = mc% + 1
      IF mc% > 500 THEN mc% = 500
      cant%(mc%) = 3
      canx%(mc%) = BazX%(z%) + x%
      cany%(mc%) = BazY%(z%) + y%
    END IF
    IF x% = 0 AND y% = 0 THEN ll% = 50 ELSE ll% = 30
    IF BazX%(z%) + x% = px%(n%) AND BazY%(z%) + y% = py%(n%) THEN Life%(n%) = Life%(n%) - ll%
  NEXT x%
  NEXT y%
  NEXT n%
    BazAmmo%(z%) = BazAmmo%(z%) - 1
    tm%(6) = 1
  EXIT SUB
END IF

IF BazOn%(z%) = 1 THEN EXIT SUB

IF Ammo%(z%) < 1 THEN EXIT SUB

IF ps%(z%) = 0 THEN dr% = -1 ELSE dr% = 1
x% = px%(z%)

DO
  x% = x% + dr%
  IF ExtraAmmo%(z%) < 1 THEN Ammo%(z%) = Ammo%(z%) - 1 ELSE ExtraAmmo%(z%) = ExtraAmmo%(z%) - 1
  IF BDeadT%(2) > -1 THEN IF x% = bx%(2) AND py%(z%) = by%(2) THEN Blife%(2) = Blife%(2) - 5: EXIT DO
  IF BDeadT%(1) > -1 THEN IF x% = bx%(1) AND py%(z%) = by%(1) THEN Blife%(1) = Blife%(1) - 5: EXIT DO
  IF DeadT%(2) > -1 AND z% = 1 THEN IF x% = px%(2) AND py%(1) = py%(2) THEN Life%(2) = Life%(2) - 5: ht%(2) = 1: EXIT DO
  IF DeadT%(1) > -1 AND z% = 2 THEN IF x% = px%(1) AND py%(2) = py%(1) THEN Life%(1) = Life%(1) - 5: ht%(1) = 1: EXIT DO
  IF Map%(x%, py%(z%)) = 48 THEN Map%(x%, py%(z%)) = 52
  IF Map%(x%, py%(z%)) = 49 THEN Map%(x%, py%(z%)) = 53
  IF Map%(x%, py%(z%)) = 50 THEN Map%(x%, py%(z%)) = 48
  IF Map%(x%, py%(z%)) = 51 THEN Map%(x%, py%(z%)) = 49
  IF Map%(x%, py%(z%)) = 26 THEN Map%(x%, py%(z%)) = 28
  IF Map%(x%, py%(z%)) = 27 THEN Map%(x%, py%(z%)) = 29
  IF Map%(x%, py%(z%)) = 23 THEN Map%(x%, py%(z%)) = 0
  IF Map%(x%, py%(z%)) > 140 AND Map%(x%, py%(z%)) < 147 THEN Map%(x%, py%(z%)) = Map%(x%, py%(z%)) + 6
  IF Map%(x%, py%(z%)) > 175 AND Map%(x%, py%(z%)) < 185 THEN Map%(x%, py%(z%)) = Map%(x%, py%(z%)) + 9
  IF Map%(x%, py%(z%)) > 245 AND Map%(x%, py%(z%)) < 420 THEN Map%(x%, py%(z%)) = Map%(x%, py%(z%)) + 35
  IF Map%(x%, py%(z%)) > 420 AND Map%(x%, py%(z%)) < 700 THEN Map%(x%, py%(z%)) = 0: EXIT DO
  IF Map%(x%, py%(z%)) = 104 THEN Map%(x%, py%(z%)) = 596: Alive 1, x%, py%(z%)
  IF Map%(x%, py%(z%)) = 103 THEN Map%(x%, py%(z%)) = 606: Alive 2, x%, py%(z%)
  IF Map%(x%, py%(z%)) THEN EXIT DO
  IF ExtraAmmo%(z%) < 1 THEN Map%(x%, py%(z%)) = 33 ELSE Map%(x%, py%(z%)) = 34
  IF Ammo%(z%) < 1 THEN EXIT DO
LOOP

IF Life%(1) < 0 THEN Life%(1) = 0
IF Life%(2) < 0 THEN Life%(2) = 0
IF Ammo%(1) < 0 THEN Ammo%(1) = 0
IF Ammo%(2) < 0 THEN Ammo%(2) = 0

IF ExtraAmmo%(z%) > 0 AND ht%(2) = 1 THEN Life%(2) = Life%(2) - 5
IF ExtraAmmo%(z%) > 0 AND ht%(1) = 1 THEN Life%(1) = Life%(1) - 5

Engine

x% = px%(z%)

DO
  x% = x% + dr%
  IF Map%(x%, py%(z%)) <> 33 AND Map%(x%, py%(z%)) <> 34 THEN EXIT DO
  Map%(x%, py%(z%)) = 0
LOOP

IF Map%(x%, py%(z%)) = 169 THEN
  Map%(x%, py%(z%)) = 420
  mc% = mc% + 1
  IF mc% > 500 THEN mc% = 500
  cant%(mc%) = 1
  canx%(mc%) = x%
  cany%(mc%) = py%(z%)
END IF

Status

ht%(1) = 0: ht%(2) = 0

'=============================================================================

END SUB

SUB Status

IF Life%(1) < 0 THEN Life%(1) = 0
IF Life%(2) < 0 THEN Life%(2) = 0
Bife%(1) = Blife%(1)
Bife%(2) = Blife%(2)
IF Bife%(1) < 0 THEN Bife%(1) = 0
IF Bife%(2) < 0 THEN Bife%(2) = 0

LOCATE 18, 8
COLOR 10
PRINT "Life "; : COLOR 15: PRINT "="; Life%(1)
LOCATE 18, 24
COLOR 10
PRINT "Life "; : COLOR 15: PRINT "="; Life%(2)

LOCATE 19, 8
COLOR 2
PRINT "Ammo "; : COLOR 15: PRINT "="; Ammo%(1)
LOCATE 19, 24
COLOR 2
PRINT "Ammo "; : COLOR 15: PRINT "="; Ammo%(2)

LOCATE 20, 7
COLOR 7
PRINT "Mines "; : COLOR 15: PRINT "="; Mines%(1)
LOCATE 20, 23
COLOR 7
PRINT "Mines "; : COLOR 15: PRINT "="; Mines%(2)

LOCATE 21, 8
COLOR 204
PRINT "Toad "; : COLOR 15: PRINT "="; Bife%(1)
LOCATE 21, 24
COLOR 74
PRINT "Toad "; : COLOR 15: PRINT "="; Bife%(2)

LOCATE 22, 4
COLOR 8
IF Baz%(1) = 1 THEN PRINT "Baz Ammo "; : COLOR 15: PRINT "="; BazAmmo%(1)
LOCATE 22, 20
COLOR 8
IF Baz%(2) = 1 THEN PRINT "Baz Ammo "; : COLOR 15: PRINT "="; BazAmmo%(2)

FOR z% = 1 TO 2
FOR n% = 1 TO kl%(z%)
IF z% = 1 THEN p% = 0 ELSE p% = 126
IF KeyCard%(n%, z%) = 106 THEN PUT (8 * 8 - 8 + p%, 122), sprites%(eps% * 106), PSET
IF KeyCard%(n%, z%) = 108 THEN PUT (9 * 8 - 8 + p%, 122), sprites%(eps% * 108), PSET
IF KeyCard%(n%, z%) = 110 THEN PUT (10 * 8 - 8 + p%, 122), sprites%(eps% * 110), PSET
IF KeyCard%(n%, z%) = 112 THEN PUT (11 * 8 - 8 + p%, 122), sprites%(eps% * 112), PSET
IF KeyCard%(n%, z%) = 114 THEN PUT (12 * 8 - 8 + p%, 122), sprites%(eps% * 114), PSET
IF KeyCard%(n%, z%) = 116 THEN PUT (13 * 8 - 8 + p%, 122), sprites%(eps% * 116), PSET
NEXT n%
NEXT z%

FOR z% = 1 TO 2
  IF z% = 1 THEN p% = 0 ELSE p% = 126
  PUT (15 * 8 - 8 + p%, 122), sprites%(eps% * Weapon%(z%)), PSET
  IF Sweeper%(z%) = 1 THEN PUT (16 * 8 - 8 + p%, 122), sprites%(eps% * 64), PSET
  IF Baz%(z%) = 1 THEN PUT (17 * 8 - 8 + p%, 122), sprites%(eps% * 68), PSET
NEXT z%

END SUB

SUB Stuff

IF Map%(px%(1), py%(1)) = 0 THEN Map%(px%(1), py%(1)) = 1
IF Map%(px%(2), py%(2)) = 0 THEN Map%(px%(2), py%(2)) = 1
IF Map%(bx%(1), by%(1)) = 0 THEN Map%(bx%(1), by%(1)) = 1
IF Map%(bx%(2), by%(2)) = 0 THEN Map%(bx%(2), by%(2)) = 1

  IF Map%(opx%(1), opy%(1)) = 1 THEN Map%(opx%(1), opy%(1)) = 0
  IF Map%(opx%(2), opy%(2)) = 1 THEN Map%(opx%(2), opy%(2)) = 0
  IF Map%(px%(1), py%(1)) = 1 THEN Map%(px%(1), py%(1)) = 0
  IF Map%(px%(2), py%(2)) = 1 THEN Map%(px%(2), py%(2)) = 0
  IF Map%(obx%(1), oby%(1)) = 1 THEN Map%(obx%(1), oby%(1)) = 0
  IF Map%(obx%(2), oby%(2)) = 1 THEN Map%(obx%(2), oby%(2)) = 0
  IF Map%(bx%(1), by%(1)) = 1 THEN Map%(bx%(1), by%(1)) = 0
  IF Map%(bx%(2), by%(2)) = 1 THEN Map%(bx%(2), by%(2)) = 0


END SUB

