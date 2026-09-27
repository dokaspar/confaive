'
'          ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ
'          Û                                                   Û
'          Û  ÛÛÛÛÛ  ÛÛÛ  ÛÛ  Û ÛÛÛÛÛ ÛÛÛÛ   ÛÛÛ  Û  ÛÛ ÛÛÛÛÛ  Û
'          Û  Û  ÛÛ Û   Û ÛÛÛ Û Û   Û Û  ÛÛ   Û   Û   Û Û   Û  Û
'          Û  Û     Û   Û Û ÛÛÛ ÛÛÛ   Û   Û   Û   Û   Û ÛÛÛ    Û
'          Û  Û     Û   Û Û  ÛÛ Û     ÛÛÛÛÛ   Û   ÛÛ ÛÛ Û      Û
'          Û  ÛÛ  Û Û   Û ÛÛ  Û ÛÛ    ÛÛ  Û  ÛÛ    ÛÛÛ  ÛÛ  Û  Û
'          Û  ÛÛÛÛÛ  ÛÛÛ  ÛÛ ÛÛ ÛÛ    ÛÛ ÛÛ  ÛÛÛ    Û   ÛÛÛÛÛ  Û
'          Û                                                   Û
'          ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ
'
'          CONNECT FIVE WITH ARTIFICIAL INTELLIGENCE VERSION 1.0
'
'          PROGRAMMED 1999 BY KASWOJ SOFTWARE
'          http://www.geocities.com/CapeCanaveral/Cockpit/3406/
'          kaswoj@gmx.net
'
'          Game Routines > Dominik Kaspar
'                          Ahornweg 15
'                          5615 Fahrwangen
'                          Switzerland
'                          pedok@pop.agri.ch
'
'          Thanks to:    > Jesse Dorland for QMIDI V4.1
'                          jessedorland@hotmail.com
'
'                        > Stefan A. Dittrich for his Mouse Routines
'
'                        > unknown programmer for BMP loader


'--CONFAIVE GAME ROUTINES--'
   DECLARE SUB About ()
   DECLARE SUB Button (x!, y!, xx!, yy!, c)
   DECLARE SUB ButtonPressed (x!, y!, xx!, yy!, c!)
   DECLARE SUB CalculateBest ()
   DECLARE SUB ChangeAI ()
   DECLARE SUB ChangeGrid ()
   DECLARE SUB ChangeMusic ()
   DECLARE SUB ChangePlayers ()
   DECLARE SUB CheckPlaySong ()
   DECLARE SUB ClearAround (x AS INTEGER, y AS INTEGER, Border AS INTEGER)
   DECLARE SUB ClearBoard ()
   DECLARE SUB Delay (tdelay!)
   DECLARE SUB DetectWorkingDir (DataFile$)
   DECLARE SUB DrawBoard ()
   DECLARE SUB GameOver (p AS INTEGER)
   DECLARE SUB HelpScreen ()
   DECLARE SUB Hint ()
   DECLARE SUB InitializeFont ()
   DECLARE SUB Intro ()
   DECLARE SUB MenuBar ()
   DECLARE SUB Newgame ()
   DECLARE SUB PlayGame ()
   DECLARE SUB PrintStr (XPos!, YPos!, Colour!, Text$)
   DECLARE SUB ReadMenuBar ()
   DECLARE SUB ReadMoveList ()
   DECLARE SUB ReadOptions ()
   DECLARE SUB ReadStats ()
   DECLARE SUB ReallyQuit ()
   DECLARE SUB SetDefault ()
   DECLARE SUB StatShow ()
   DECLARE SUB StatReset ()
   DECLARE SUB StoppedGame ()
   DECLARE SUB Undo ()
   DECLARE SUB UpdateScreen ()
   DECLARE SUB WhoIsWinner ()
   DECLARE FUNCTION CalculatePoints! (x!, y!, st1, st2)
   DECLARE FUNCTION CountSymbol! (Text$, Symbol$)
   DECLARE FUNCTION Decode$ (Text$)
   DECLARE FUNCTION Density! (x!, y!)
   DECLARE FUNCTION Encode$ (Text$)
   DECLARE FUNCTION Prompt$ (XPos!, YPos!, c!, Text$, Limit!)
   DECLARE FUNCTION WinCheck! (p AS INTEGER)

'--QMIDI V4.1 ROUTINES--'
   DECLARE SUB CleanUpMidi ()
   DECLARE SUB DriversLoaded (SBMIDI%, SBSIM%)
   DECLARE SUB InternalGetIntVector (IntNum%, segment&, OFFSET&)
   DECLARE SUB IntX (IntNum AS INTEGER, Regs AS ANY)
   DECLARE SUB LoopMIDI ()
   DECLARE SUB PauseMIDI ()
   DECLARE SUB PlayMidi (Handle%)
   DECLARE SUB ResumeMIDI ()
   DECLARE SUB StopMidi ()
   DECLARE SUB UnloadMidi (Handle%)
   DECLARE FUNCTION GetSynth% ()
   DECLARE FUNCTION LoadMIDI% (FileName$)
   DECLARE FUNCTION MemUsed& (Handle%)
   DECLARE FUNCTION MIDIError$ ()
   DECLARE FUNCTION MusicDone% ()
   DECLARE FUNCTION TimeMIDI! ()
  
'--MOUSE ROUTINES--'
   DECLARE SUB GetMouse ()
   DECLARE SUB Mouse (OnOff%)
   DECLARE SUB MouseInit ()
   DECLARE SUB MouseSet (x%, y%)
   DECLARE SUB MouseXLim (x1%, x2%)
   DECLARE SUB MouseYLim (y1%, y2%)
   DECLARE SUB ReadData ()
   DECLARE SUB WaitKeyUp ()
   DECLARE SUB WaitMove ()
   DECLARE FUNCTION Interr% (num%, AX%, BX%, cx%, dx%)

'--BMP LOADER--'
   DECLARE SUB ShowBMP (data$)

'--QBMIDI VARIABLES--'
   '$DYNAMIC
   TYPE Registers
	AX    AS INTEGER
	BX    AS INTEGER
	cx    AS INTEGER
	dx    AS INTEGER
	BP    AS INTEGER
	SI    AS INTEGER
	DI    AS INTEGER
	FLAGS AS INTEGER
	DS    AS INTEGER
	ES    AS INTEGER
   END TYPE
IntXCodeData:
   DATA  &H55, &H8B, &HEC, &H83, &HEC, &H08, &H56, &H57, &H1E, &H55, &H8B, &H5E
   DATA  &H06, &H8B, &H47, &H10, &H3D, &HFF, &HFF, &H75, &H04, &H1E, &H8F, &H47
   DATA  &H10, &H8B, &H47, &H12, &H3D, &HFF, &HFF, &H75, &H04, &H1E, &H8F, &H47
   DATA  &H12, &H8B, &H47, &H08, &H89, &H46, &HF8, &H8B, &H07, &H8B, &H4F, &H04
   DATA  &H8B, &H57, &H06, &H8B, &H77, &H0A, &H8B, &H7F, &H0C, &HFF, &H77, &H12
   DATA  &H07, &HFF, &H77, &H02, &H1E, &H8F, &H46, &HFA, &HFF, &H77, &H10, &H1F
   DATA  &H8B, &H6E, &HF8, &H5B, &HCD, &H21, &H55, &H8B, &HEC, &H8B, &H6E, &H02
   DATA  &H89, &H5E, &HFC, &H8B, &H5E, &H06, &H1E, &H8F, &H46, &HFE, &HFF, &H76
   DATA  &HFA, &H1F, &H89, &H07, &H8B, &H46, &HFC, &H89, &H47, &H02, &H89, &H4F
   DATA  &H04, &H89, &H57, &H06, &H58, &H89, &H47, &H08, &H89, &H77, &H0A, &H89
   DATA  &H7F, &H0C, &H9C, &H8F, &H47, &H0E, &H06, &H8F, &H47, &H12, &H8B, &H46
   DATA  &HFE, &H89, &H47, &H10, &H5A, &H1F, &H5F, &H5E, &H8B, &HE5, &H5D, &HCA
   DATA  &H02, &H00
   DIM SHARED QMIDIRegs AS Registers, MEM.SEGMENT(0 TO 255) AS INTEGER
   DIM SHARED MIDI.PLAYTIME AS SINGLE, MIDI.ERROR AS INTEGER, PAUSED AS SINGLE
   DIM SHARED SBMIDI.INTERRUPT AS INTEGER, MEM.ALLOCATED(0 TO 255) AS LONG
   DIM SHARED SBSIM.INTERRUPT AS INTEGER, MIXER.CHIP AS INTEGER
   DIM SHARED SB.BASEPORT AS INTEGER, SB.IRQ AS INTEGER
   DIM SHARED SB.LODMA AS INTEGER, SB.HIDMA AS INTEGER, SB.CARDTYPE AS INTEGER
   DIM SHARED SB.MPU401 AS INTEGER, BIT.STORAGE(0 TO 7) AS INTEGER
   DIM SHARED SENSITIVE AS INTEGER, REVERSE.STEREO AS INTEGER
   DIM SHARED SOUND.DISABLED AS INTEGER, CURRENTHANDLE AS INTEGER

'BEGINNING OF GAME CODE OF
'CONNECT FIVE WITH ARTIFICIAL INTELLIGENCE (AKA CONFAIVE)

TYPE MoveListFileType
     XElement AS INTEGER
     YElement AS INTEGER
END TYPE
DIM SHARED MoveListFile AS MoveListFileType

TYPE StatType
     Wins    AS INTEGER
     Losses  AS INTEGER
     StreakW AS INTEGER
     StreakL AS INTEGER
     StreakC AS INTEGER
     Best    AS INTEGER
     Worst   AS INTEGER
     Stopped AS INTEGER
END TYPE
DIM SHARED Stat AS StatType

TYPE OptType
     oP1C  AS INTEGER
     oP1S  AS STRING * 1
     oP2C  AS INTEGER
     oP2S  AS STRING * 1
     oMid1 AS INTEGER
     oMid2 AS INTEGER
     oMid3 AS INTEGER
     oMid4 AS INTEGER
     oMid5 AS INTEGER
     oMid6 AS INTEGER
     oMidiFile AS INTEGER
     oPlayMode AS INTEGER
     oStrategy AS INTEGER
     oRandomIQ AS INTEGER
     oDensCalc AS INTEGER
     oGridMode AS INTEGER
     oTurn     AS INTEGER
     oMoves    AS INTEGER
END TYPE
DIM SHARED Opt AS OptType

'Declaration of global variables and arrays for the game:

'Board has a size of 30x18 with an invisible border of 4:
 DIM SHARED Board(-3 TO 34, -3 TO 22) AS INTEGER
	    '(x, y) = 0: Empty field    2: Computer player
	    '         1: Human player   9: Border
	    '         8: square that needn't be checked (speeds up game)

'Colors and symbols of players:
 DIM SHARED P1C, P2C
 DIM SHARED P1S$, P2S$

'Array to store moves for undo function:
 DIM SHARED MoveList(1 TO 540, 1 TO 2) AS INTEGER

'Variable to hold score values:
 DIM SHARED Score(1 TO 30, 1 TO 18)

'Variables that hold best square to take:
 DIM SHARED BestX AS INTEGER, BestY AS INTEGER

'[0]=Human/Computer  [1]=Computer/Computer  [2]=Human/Human
 DIM SHARED PlayMode AS INTEGER

'Variable which tells the computer to play with a balanced [0],
'a defensive [1] or an offensive [2] strategy.
 DIM SHARED Strategy AS INTEGER

'Midi file playing:
 DIM SHARED MidiFX AS INTEGER
 DIM SHARED MidiFile AS INTEGER
 DIM SHARED MidiChk(0 TO 6) AS INTEGER
 DIM SHARED MidiHandle AS INTEGER

'Other variables:
 DIM SHARED RandomIQ AS INTEGER
 DIM SHARED Moves AS INTEGER
 DIM SHARED Turn  AS INTEGER     'Turn = 1 (player)   Turn = 2 (computer)
 DIM SHARED DensCalc AS INTEGER
 DIM SHARED GridMode AS INTEGER

SCREEN 0
WIDTH 80, 25
COLOR 14, 1
CLS
RANDOMIZE TIMER

PRINT
PRINT " ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
PRINT " º CONFAIVE - Connect 5 with artificial intelligence º"
PRINT " º            1999 by Dominik Kaspar                 º"
PRINT " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
PRINT

'Mouse Initialization:
 DIM SHARED MS%(45)
 DIM SHARED MouseX, MouseY, MouseK
 PRINT " > Mouse Initialization...";
 ReadData
 MouseInit
 MouseXLim 0, 319
 MouseYLim 0, 200
 PRINT "[OK]"

'Searching path where program files are stored:
 DIM SHARED CurDir$
 PRINT " > Detecting Working Directory...";
 DetectWorkingDir "CONFAIVE.FNT"
 PRINT "[OK]"

'Font Initialization:
 DIM SHARED Font(0 TO 127, 1 TO 6) AS INTEGER
 PRINT " > Loading Font...";
 InitializeFont
 PRINT "[OK]"

'Reading in menu bar items:
 PRINT " > Reading Menu Bar Items...";
 DIM SHARED mnuItem(1 TO 4, 1 TO 9) AS STRING * 10
 DIM SHARED hlpItem(1 TO 4, 1 TO 9) AS STRING * 50
 ReadMenuBar
 PRINT "[OK]"

'Reading in statistical data
 PRINT " > Loading Statistics...";
 ReadStats
 PRINT "[OK]"

'Reading in data of game options:
 PRINT " > Loading Game Options...";
 ReadOptions
 PRINT "[OK]"

PRINT
PRINT " ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
PRINT " º Make a selection:         º"
PRINT " º                           º"
PRINT " º [1] MIDI BACKGROUND MUSIC º"
PRINT " º [2] NO MUSIC              º"
PRINT " ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
PRINT
i = 0
DO: i = VAL(INKEY$): LOOP UNTIL i = 1 OR i = 2
IF i = 1 THEN MidiFX = 1 ELSE MidiFX = 0

'Midi Detection:
 IF MidiFX THEN
    PRINT " > Loading Midi Drivers...";
    DriversLoaded SBMIDI.INTERRUPT, SBSIM.INTERRUPT
    IF SBMIDI.INTERRUPT = 0 THEN SBMIDI.INTERRUPT = &H80
    IF SBSIM.INTERRUPT = 0 THEN SBSIM.INTERRUPT = &H81
    PRINT "[OK]"
 END IF

Delay 1
Intro          'Display of Logo

'Play song if user has chosen any:
CheckPlaySong

SCREEN 7

ClearBoard      'Empty the board
DrawBoard       'Draw the emptied board to the screen
ReadMoveList    'Reads file with saved game

PlayGame        'Main Game Routine

Mouse 0
ReallyQuit
SYSTEM

'--MOUSE DATA--'
MS.Data:
DATA 55,8b,ec,56,57
DATA 8b,76,0c,8b,04
DATA 8b,76,0a,8b,1c
DATA 8b,76,08,8b,0c
DATA 8b,76,06,8b,14
DATA cd,21
DATA 8b,76,0c,89,04
DATA 8b,76,0a,89,1c
DATA 8b,76,08,89,0c
DATA 8b,76,06,89,14
DATA 5f,5e,5d
DATA ca,08,00
DATA #

REM $STATIC
SUB About
 Mouse 0
 PCOPY 1, 0

 Button 0, 0, 319, 199, 0
 PrintStr 20, 20, 15, "CONFAIVE - Version 1.0"
 PrintStr 20, 32, 15, "Connect Five with artificial intelligence"
 PrintStr 20, 50, 7, "Programmed 1999 by KASWOJ SOFTWARE"
 PrintStr 20, 65, 7, "Game Routines  > Dominik Kaspar"
 PrintStr 20, 80, 7, "Thanks to:     > Jesse Dorland"
 PrintStr 20, 90, 7, "                 for his QMIDI V4.1"
 PrintStr 20, 105, 7, "               > Stefan A. Dittrich"
 PrintStr 20, 115, 7, "                 for the mouse routines"
 PrintStr 20, 130, 7, "Contact:"
 PrintStr 20, 145, 7, "www.geocities.com/CapeCanaveral/Cockpit/3406"
 PrintStr 20, 155, 7, "e-mail: kaswoj@gmx.net"
 PrintStr 20, 172, 7, "this game is freeware. Have fun!"

 Mouse 1                        'show cursor of mouse
 DO
   WaitMove
 LOOP UNTIL MouseK
END SUB

SUB Button (x, y, xx, yy, c)
 LINE (x, y)-(xx, yy), 0, B
 LINE (x, y)-(xx - 1, yy - 1), 7, B
 LINE (x + 1, y + 1)-(xx - 1, yy - 1), 8, B
 LINE (x + 1, y + 1)-(xx - 2, yy - 2), 15, B
 LINE (x + 2, y + 2)-(xx - 2, yy - 2), 7, B
 LINE (x + 2, y + 2)-(xx - 3, yy - 3), c, BF
END SUB

SUB ButtonPressed (x, y, xx, yy, c)
 LINE (x, y)-(xx, yy), 0, B
 LINE (x, y)-(xx - 1, yy - 1), 8, B
 LINE (x + 1, y + 1)-(xx - 1, yy - 1), 7, B
 LINE (x + 1, y + 1)-(xx - 2, yy - 2), 7, B
 LINE (x + 2, y + 2)-(xx - 2, yy - 2), 15, B
 LINE (x + 2, y + 2)-(xx - 3, yy - 3), c, BF
END SUB

SUB CalculateBest

'Every single square will be checked and graded indiviually.
'The square with the highest score will then be stored as BestX and BestY:

 IF Moves = 0 THEN
    BestX = INT(RND * 8) + 12
    BestY = INT(RND * 6) + 6
    ClearAround BestX, BestY, 1
    EXIT SUB
 END IF

 IF INT(RND * 101) > RandomIQ THEN       'Random IQ test
    FOR x = 1 TO 30
	FOR y = 1 TO 18
	    Score(x, y) = 0
	    IF Board(x, y) = 0 THEN Score(x, y) = INT(RND * 1000)
	NEXT y
    NEXT x
    GOTO FindBest
 END IF

 st1 = 0: st2 = 0
 SELECT CASE Turn
   CASE 1: SELECT CASE Strategy
		  CASE 1: st1 = -1: st2 = 1
		  CASE 2: st1 = 1: st2 = -1
	   END SELECT
   CASE 2: SELECT CASE Strategy
		  CASE 1: st1 = 1: st2 = -1
		  CASE 2: st1 = -1: st2 = 1
	   END SELECT
 END SELECT

 FOR x = 1 TO 30                            'start of main loops
     FOR y = 1 TO 18
	 IF Board(x, y) = 0 THEN
	    Score(x, y) = CalculatePoints(x, y, st1, st2)  'score calculation
	    Score(x, y) = Score(x, y) + INT(RND * 1000)    'add some random
	 ELSE
	    Score(x, y) = 0
	 END IF
     NEXT y
 NEXT x

FindBest:                                    'find square with highest score
 DIM hi(1 TO 8, 1 TO 3)
 FOR x = 1 TO 30
     FOR y = 1 TO 18
	 IF Score(x, y) > hi(1, 3) THEN
	    FOR i = 1 TO 8
		hi(i, 1) = 0
		hi(i, 2) = 0
		hi(i, 3) = 0
	    NEXT i
	    hi(1, 1) = x
	    hi(1, 2) = y
	    hi(1, 3) = Score(x, y)
	 ELSEIF Score(x, y) = hi(1, 3) THEN
	    FOR i = 1 TO 8
		IF hi(i, 3) = 0 THEN
		   hi(i, 1) = x
		   hi(i, 2) = y
		   hi(i, 3) = Score(x, y)
		   EXIT FOR
		END IF
	    NEXT i
	 END IF
     NEXT y
 NEXT x

 cnt = 0                                     'if there are several squares
 FOR i = 1 TO 8                              'with the same score, then
     IF hi(i, 3) > 0 THEN cnt = cnt + 1      'choose a random one.
 NEXT i
 i = INT(RND * cnt) + 1
 BestX = hi(i, 1)
 BestY = hi(i, 2)
 'LOCATE 1, 34: PRINT LTRIM$(RTRIM$(STR$(Score(BestX, BestY))));
 'LOCATE 2, 34: PRINT BestX; BestY

 ClearAround BestX, BestY, 1
END SUB

FUNCTION CalculatePoints (x, y, st1, st2)
 DIM Dir1(1 TO 4) AS STRING * 9             'holds 4 "directions" for
 DIM Dir2(1 TO 4) AS STRING * 9             'human player and computer

 Points = 0
 PossCorner1 = 0: PossCorner2 = 0

'fill up four strings with "direction segments" surrounding (x,y):
 FOR i = 1 TO 9
     MID$(Dir1(1), i, 1) = LTRIM$(STR$(Board(x - 5 + i, y + 5 - i)))
     MID$(Dir1(2), i, 1) = LTRIM$(STR$(Board(x - 5 + i, y)))
     MID$(Dir1(3), i, 1) = LTRIM$(STR$(Board(x - 5 + i, y - 5 + i)))
     MID$(Dir1(4), i, 1) = LTRIM$(STR$(Board(x, y - 5 + i)))
 NEXT i

 FOR i = 1 TO 4
     Dir2(i) = Dir1(i)           'same string for Dir2
 NEXT i

'set middle placeholder to future value:
 FOR i = 1 TO 4
     MID$(Dir1(i), 5, 1) = "1"
     MID$(Dir2(i), 5, 1) = "2"
 NEXT i

'replace unneeded placeholders with a "9" character:
 FOR j = 1 TO 4
     l1 = 0: R1 = 0
     l2 = 0: R2 = 0
     FOR i = 1 TO 4
	 IF MID$(Dir1(j), 5 + i, 1) = "2" THEN R1 = 1
	 IF MID$(Dir1(j), 5 - i, 1) = "2" THEN l1 = 1
	 IF R1 = 1 THEN MID$(Dir1(j), 5 + i, 1) = "9"
	 IF l1 = 1 THEN MID$(Dir1(j), 5 - i, 1) = "9"
	 IF MID$(Dir2(j), 5 + i, 1) = "1" THEN R2 = 1
	 IF MID$(Dir2(j), 5 - i, 1) = "1" THEN l2 = 1
	 IF R2 = 1 THEN MID$(Dir2(j), 5 + i, 1) = "9"
	 IF l2 = 1 THEN MID$(Dir2(j), 5 - i, 1) = "9"
     NEXT i
   'treatment of special cases:
    IF INSTR(Dir1(j), "011110") OR INSTR(Dir2(j), "022220") THEN
       Points = Points + 2 * 4 ^ 8
    END IF
    IF INSTR(Dir1(j), "01110") THEN
       PossCorner1 = PossCorner1 + 1
       IF PossCorner1 >= 2 THEN Points = Points + 2 * 4 ^ 8
    END IF
    IF INSTR(Dir1(j), "02220") THEN
       PossCorner2 = PossCorner2 + 1
       IF PossCorner2 >= 2 THEN Points = Points + 2 * 4 ^ 8
    END IF
    IF INSTR(Dir1(j), "911119") OR INSTR(Dir2(j), "922229") THEN
       Points = Points - 2 * 4 ^ 8
    END IF
    IF INSTR(Dir1(j), "91119") OR INSTR(Dir2(j), "92229") THEN
       Points = Points - 2 * 3 ^ 8
    END IF
 NEXT j

'distribution of points:
 FOR j = 1 TO 4
     FOR i = 1 TO 5
	 p = CountSymbol(MID$(Dir1(j), i, 5), "1")
	 Points = Points + p ^ 8 + st1 * p ^ 4
	 IF Turn = 1 AND p = 5 THEN p = 6
	 p = CountSymbol(MID$(Dir2(j), i, 5), "2")
	 IF Turn = 2 AND p = 5 THEN p = 6
	 Points = Points + p ^ 8 + st2 * p ^ 4
     NEXT i
 NEXT j

'Other criterias:
'The closer a square is to other set squares, the more points it should get:
 IF DensCalc THEN Points = Points + Density(x, y)

 CalculatePoints = Points
END FUNCTION

SUB ChangeAI
 Mouse 0                        'hide mouse cursor
 PCOPY 1, 0                     'redraw game screen

 Button 49, 29, 270, 170, 0     'draw message box

 PrintStr 60, 40, 7, "Artificial IQ  (Random)"
 Button 60, 55, 162, 67, 0
 LINE (62, 57)-(62 + RandomIQ / 100 * 97, 64), 9, BF
 Button 170, 55, 182, 67, 7
 Button 190, 55, 202, 67, 7
 PrintStr 173, 58, 0, "<"
 PrintStr 194, 58, 0, ">"

 PrintStr 60, 78, 7, "Strategy:"
 FOR i = 1 TO 3
     Button 123, 60 + i * 15, 134, 72 + i * 15, 0
     IF Strategy = i - 1 THEN PrintStr 125, 63 + i * 15, 9, "X"
 NEXT i
 PrintStr 140, 78, 7, "Balanced"
 PrintStr 140, 93, 7, "Defensive"
 PrintStr 140, 108, 7, "Offensive"

 Button 60, 122, 71, 134, 0
 IF DensCalc = 1 THEN PrintStr 62, 125, 9, "X"
 PrintStr 75, 125, 7, "Special Density Calculation"

 Button 140, 141, 180, 160, 7
 PrintStr 153, 148, 0, "OK"

 Mouse 1                        'show mouse cursor
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
     IF MouseK = 1 THEN
	WaitKeyUp
	Mouse 0
	IF x > 170 AND y > 55 AND x < 182 AND y < 67 THEN
	   RandomIQ = RandomIQ - 10
	   IF RandomIQ < 0 THEN RandomIQ = 0
	   LINE (62, 57)-(159, 64), 0, BF
	   LINE (62, 57)-(62 + RandomIQ / 100 * 97, 64), 9, BF
	   ButtonPressed 170, 55, 182, 67, 7
	   PrintStr 173, 58, 0, "<"
	   Delay .1
	   Button 170, 55, 182, 67, 7
	   PrintStr 173, 58, 0, "<"
	END IF
	IF x > 190 AND y > 55 AND x < 202 AND y < 67 THEN
	   RandomIQ = RandomIQ + 10
	   IF RandomIQ > 100 THEN RandomIQ = 100
	   LINE (62, 57)-(62 + RandomIQ / 100 * 97, 64), 9, BF
	   ButtonPressed 190, 55, 202, 67, 7
	   PrintStr 194, 58, 0, ">"
	   Delay .1
	   Button 190, 55, 202, 67, 7
	   PrintStr 194, 58, 0, ">"
	END IF
	IF x > 123 AND x < 134 THEN
	   SELECT CASE y
	     CASE 75 TO 87: Strategy = 0
	     CASE 90 TO 102: Strategy = 1
	     CASE 105 TO 115: Strategy = 2
	   END SELECT
	   FOR i = 1 TO 3
	       Button 123, 60 + i * 15, 134, 72 + i * 15, 0
	       LINE (125, 60 + i * 15 + 2)-(131, 72 + i * 15 - 3), 0, BF
	       IF Strategy = i - 1 THEN PrintStr 125, 63 + i * 15, 9, "X"
	   NEXT i
	END IF
	IF x > 60 AND y > 122 AND x < 71 AND y < 134 THEN
	   IF DensCalc = 1 THEN
	      DensCalc = 0
	      LINE (62, 124)-(68, 131), 0, BF
	   ELSE
	      DensCalc = 1
	      PrintStr 62, 125, 9, "X"
	   END IF
	END IF
	IF x > 140 AND y > 141 AND x < 180 AND y < 160 THEN
	   ButtonPressed 140, 141, 180, 160, 7
	   PrintStr 153, 148, 0, "OK"
	   Delay .5
	   Mouse 1
	   EXIT SUB
	END IF
	Mouse 1
     END IF
 LOOP

END SUB

SUB ChangeGrid
 Mouse 0
 PCOPY 1, 0

 Button 95, 45, 225, 155, 0
 PrintStr 130, 60, 7, "Simple Lines"
 PrintStr 130, 75, 7, "Dotted Lines"
 PrintStr 130, 90, 7, "Chess Board"
 PrintStr 130, 105, 7, "No Grid Lines"

 FOR i = 0 TO 3
     Button 110, 57 + 15 * i, 121, 69 + 15 * i, 0
     IF GridMode = i THEN PrintStr 112, 60 + 15 * i, 9, "X"
 NEXT i

 Button 140, 122, 180, 142, 7
 PrintStr 153, 129, 0, "OK"

 Mouse 1                        'show mouse cursor
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
     IF MouseK = 1 THEN
	WaitKeyUp
	Mouse 0
	IF x > 110 AND x < 121 THEN
	   SELECT CASE y
	     CASE 57 TO 69: GridMode = 0
	     CASE 72 TO 84: GridMode = 1
	     CASE 87 TO 99: GridMode = 2
	     CASE 102 TO 114: GridMode = 3
	   END SELECT
	   FOR i = 0 TO 3
	       LINE (112, 59 + 15 * i)-(118, 66 + 15 * i), 0, BF
	       IF GridMode = i THEN PrintStr 112, 60 + 15 * i, 9, "X"
	   NEXT i
	END IF
	IF x > 140 AND y > 122 AND x < 180 AND y < 142 THEN
	   ButtonPressed 140, 122, 180, 142, 7
	   PrintStr 153, 129, 0, "OK"
	   Delay .5
	   UpdateScreen
	   EXIT DO
	END IF
	Mouse 1
     END IF
 LOOP
 Mouse 1
END SUB

SUB ChangeMusic

 Mouse 0
 PCOPY 1, 0

 Button 85, 30, 235, 170, 0
 PrintStr 120, 45, 7, "Blink 182 #1"
 PrintStr 120, 60, 7, "Blink 182 #2"
 PrintStr 120, 75, 7, "Reel Big Fish"
 PrintStr 120, 90, 7, "NOFX #1"
 PrintStr 120, 105, 7, "NOFX #2"
 PrintStr 120, 120, 7, "Mighty Bosstones"

 FOR i = 0 TO 5
     Button 100, 42 + 15 * i, 111, 54 + 15 * i, 0
     IF MidiChk(i + 1) = 1 THEN PrintStr 102, 45 + 15 * i, 9, "X"
 NEXT i

 Button 140, 137, 180, 157, 7
 PrintStr 153, 144, 0, "OK"

 Mouse 1                        'show mouse cursor
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
     IF MouseK = 1 THEN
	WaitKeyUp
	Mouse 0
	IF x > 100 AND x < 111 THEN
	   i = INT((y - 41) / 15 + 1)
	   IF i > 0 AND i < 7 THEN
	      IF MidiChk(i) = 1 THEN MidiChk(i) = 0 ELSE MidiChk(i) = 1
	      FOR i = 0 TO 5
		  Button 100, 42 + 15 * i, 111, 54 + 15 * i, 0
		  IF MidiChk(i + 1) = 1 THEN PrintStr 102, 45 + 15 * i, 9, "X"
	      NEXT i
	   END IF
	END IF
	IF x > 140 AND y > 137 AND x < 180 AND y < 157 THEN
	   ButtonPressed 140, 137, 180, 157, 7
	   PrintStr 153, 144, 0, "OK"
	   Delay .5
	   IF MidiChk(MidiFile) = 0 THEN CheckPlaySong
	   EXIT DO
	END IF
	Mouse 1
     END IF
 LOOP
 Mouse 1
END SUB

SUB ChangePlayers
 Mouse 0
 PCOPY 1, 0

 Button 39, 29, 280, 170, 0
 PrintStr 50, 40, 7, "Set Player Mode:"
 FOR i = 0 TO 2
     Button 50, 51 + 15 * i, 61, 63 + 15 * i, 0
     IF i = PlayMode THEN PrintStr 52, 54 + 15 * i, 9, "X"
 NEXT i
 PrintStr 70, 55, 7, "Human    - Computer"
 PrintStr 70, 70, 7, "Computer - Computer"
 PrintStr 70, 85, 7, "Human    - Human"
 PrintStr 50, 105, 7, "Player 1:"
 PrintStr 50, 120, 7, "Player 2:"

 Button 110, 103, 121, 115, 0
 Button 110, 118, 121, 130, 0
 PrintStr 112, 106, P1C, P1S$
 PrintStr 112, 121, P2C, P2S$

 FOR i = 1 TO 15
     IF i = P1C THEN
	ButtonPressed 130 + (i - 1) * 9, 105, 130 + i * 9, 114, i
	Button 130 + (i - 1) * 9, 120, 130 + i * 9, 129, i
     ELSEIF i = P2C THEN
	Button 130 + (i - 1) * 9, 105, 130 + i * 9, 114, i
	ButtonPressed 130 + (i - 1) * 9, 120, 130 + i * 9, 129, i
     ELSE
	Button 130 + (i - 1) * 9, 105, 130 + i * 9, 114, i
	Button 130 + (i - 1) * 9, 120, 130 + i * 9, 129, i
     END IF
 NEXT i

 Button 140, 140, 180, 160, 7
 PrintStr 153, 147, 0, "OK"

 Mouse 1                        'show mouse cursor
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
     IF MouseK = 1 THEN
	WaitKeyUp
	Mouse 0
	IF x > 50 AND x < 61 THEN
	   SELECT CASE y
		  CASE 51 TO 63: PlayMode = 0
		  CASE 66 TO 78: PlayMode = 1
		  CASE 81 TO 93: PlayMode = 2
	   END SELECT
	   FOR i = 0 TO 2
	       LINE (52, 53 + 15 * i)-(58, 60 + 15 * i), 0, BF
	       IF i = PlayMode THEN PrintStr 52, 54 + 15 * i, 9, "X"
	   NEXT i
	END IF
	IF x > 110 AND y > 103 AND x < 121 AND y < 115 THEN
	   PrintStr 50, 140, 7, "type in symbol"
	   PrintStr 50, 150, 7, "& press enter"
	   LINE (112, 105)-(118, 112), 0, BF
	   P1S$ = Prompt(112, 106, P1C, "", 1)
	   LINE (50, 140)-(135, 160), 0, BF
	END IF
	IF x > 110 AND y > 118 AND x < 121 AND y < 130 THEN
	   PrintStr 50, 140, 7, "type in symbol"
	   PrintStr 50, 150, 7, "& press enter"
	   LINE (112, 120)-(118, 127), 0, BF
	   P2S$ = Prompt(112, 121, P2C, "", 1)
	   LINE (50, 140)-(135, 160), 0, BF
	END IF
	IF y > 105 AND y < 114 AND x > 130 AND x < 265 THEN
	   Button 130 + (P1C - 1) * 9, 105, 130 + P1C * 9, 114, P1C
	   P1C = INT((x - 130) / 9 + 1)
	   ButtonPressed 130 + (P1C - 1) * 9, 105, 130 + P1C * 9, 114, P1C
	   LINE (112, 105)-(118, 112), 0, BF
	   PrintStr 112, 106, P1C, P1S$
	END IF
	IF y > 120 AND y < 129 AND x > 130 AND x < 265 THEN
	   Button 130 + (P2C - 1) * 9, 120, 130 + P2C * 9, 129, P2C
	   P2C = INT((x - 130) / 9 + 1)
	   ButtonPressed 130 + (P2C - 1) * 9, 120, 130 + P2C * 9, 129, P2C
	   LINE (112, 120)-(118, 127), 0, BF
	   PrintStr 112, 121, P2C, P2S$
	END IF
	IF x > 140 AND y > 140 AND x < 180 AND y < 160 THEN
	   ButtonPressed 140, 140, 180, 160, 7
	   PrintStr 153, 147, 0, "OK"
	   Delay .5
	   UpdateScreen
	   EXIT DO
	END IF
	Mouse 1
     END IF
 LOOP
 Mouse 1
END SUB

SUB CheckPlaySong
 IF MidiFX = 0 THEN EXIT SUB

 StopMidi
 UnloadMidi MidiHandle%

 j = 0
 FOR i = 1 TO 6
     IF MidiChk(i) = 1 THEN j = 1
 NEXT i
 IF j = 0 THEN
    MidiFile = 0
    EXIT SUB
 END IF

 old = MidiFile
 FOR i = old + 1 TO 6
     IF MidiChk(i) = 1 THEN
	MidiFile = i
	EXIT FOR
     END IF
 NEXT i
 IF old = MidiFile THEN
    FOR i = 1 TO 6
	IF MidiChk(i) = 1 THEN
	   MidiFile = i
	   EXIT FOR
	END IF
    NEXT i
 END IF
 SELECT CASE MidiFile
   CASE 1: A$ = "whatage.ct5"
   CASE 2: A$ = "mandms.ct5"
   CASE 3: A$ = "shehasag.ct5"
   CASE 4: A$ = "bob.ct5"
   CASE 5: A$ = "drugood.ct5"
   CASE 6: A$ = "mmbimprs.ct5"
 END SELECT
 MidiHandle = LoadMIDI(CurDir$ + A$)
 PlayMidi MidiHandle
END SUB

SUB CleanUpMidi
FOR i% = 0 TO 255
	IF MEM.SEGMENT(i%) THEN UnloadMidi i%
NEXT i%
MIDI.ERROR = 0
END SUB

SUB ClearAround (x AS INTEGER, y AS INTEGER, Border AS INTEGER)
 FOR i = -Border TO Border
     FOR j = -Border TO Border
	 IF Board(x + i, y + j) = 8 THEN
	    Board(x + i, y + j) = 0
	 END IF
     NEXT j
 NEXT i
END SUB

SUB ClearBoard
 FOR x = -3 TO 34
     FOR y = -3 TO 22
	 IF x > 0 AND y > 0 AND x < 31 AND y < 19 THEN
	    Board(x, y) = 8
	 ELSE
	    Board(x, y) = 9
	 END IF
     NEXT y
 NEXT x
END SUB

FUNCTION CountSymbol (Text$, Symbol$)
 count = 0
 FOR i = 1 TO LEN(Text$)
     IF MID$(Text$, i, 1) = Symbol$ THEN count = count + 1
 NEXT i
 CountSymbol = count
END FUNCTION

FUNCTION Decode$ (Text$)

'This is almost the same code as in the Encode() FUNCTION. So I won't de-
'scribe it again. Just everything is done the other way round...

 t$ = ""
 FOR i = 1 TO LEN(Text$)
     ascii$ = LTRIM$(STR$(ASC(MID$(Text$, i, 1))))
     ascii$ = STRING$(3 - LEN(ascii$), "0") + ascii$
     t$ = t$ + ascii$
 NEXT i

 Text$ = t$: t$ = ""
 FOR i = 1 TO LEN(Text$) STEP 3
     t$ = t$ + MID$(Text$, i + 2, 1)
 NEXT i
 Text$ = t$: t$ = ""
 FOR i = 1 TO LEN(Text$) STEP 3
     t$ = t$ + CHR$(VAL(MID$(Text$, i, 3)))
 NEXT i

 Decode$ = t$
END FUNCTION

DEFINT A-Z
SUB Delay (tdelay!)
 time1! = TIMER
 DO
 LOOP WHILE (TIMER - time1! < tdelay!) OR (time1! > TIMER)
END SUB

DEFSNG A-Z
FUNCTION Density (x, y)
 PPoints = 0
 CPoints = 0

 FOR i = 4 TO 1 STEP -1
     FOR j = (x - i) TO (x + i)
	 SELECT CASE Board(j, y + i)
		CASE 1: PPoints = PPoints + (5 - i) ^ 2
		CASE 2: CPoints = CPoints + (5 - i) ^ 2
	 END SELECT
	 SELECT CASE Board(j, y - i)
		CASE 1: PPoints = PPoints + (5 - i) ^ 2
		CASE 2: CPoints = CPoints + (5 - i) ^ 2
	 END SELECT
     NEXT j
     FOR j = (y - i + 1) TO (y + i - 1)
	 SELECT CASE Board(x - i, j)
		CASE 1: PPoints = PPoints + (5 - i) ^ 2
		CASE 2: CPoints = CPoints + (5 - i) ^ 2
	 END SELECT
	 SELECT CASE Board(x + i, j)
		CASE 1: PPoints = PPoints + (5 - i) ^ 2
		CASE 2: CPoints = CPoints + (5 - i) ^ 2
	 END SELECT
     NEXT j
 NEXT i

 SELECT CASE Turn
   CASE 1: SELECT CASE Strategy
	     CASE 0: Points = (3 / 2) * PPoints + (3 / 2) * CPoints
	     CASE 1: Points = PPoints + 2 * CPoints  'max points: 1200
	     CASE 2: Points = 2 * PPoints + CPoints
	   END SELECT
   CASE 2: SELECT CASE Strategy
	     CASE 0: Points = (3 / 2) * PPoints + (3 / 2) * CPoints
	     CASE 1: Points = 2 * PPoints + CPoints
	     CASE 2: Points = PPoints + 2 * CPoints
	   END SELECT
 END SELECT

 Density = CINT(Points * 2)
END FUNCTION

SUB DetectWorkingDir (DataFile$)
 CurDir$ = ""

 OPEN DataFile$ FOR RANDOM AS #1 LEN = LEN(Font)
      IF LOF(1) > 0 THEN
	 CLOSE #1
	 EXIT SUB
      END IF
 CLOSE #1
 KILL DataFile$

 SHELL "CD\"
 SHELL "DIR " + DataFile$ + " /S /B > CONFAIVE.TMP"

 OPEN "CONFAIVE.TMP" FOR INPUT AS #1

 IF LOF(1) = 0 THEN
    PRINT : PRINT
    PRINT " ERROR: Could not detect working directory."
    PRINT "        There is a file missing: " + DataFile$
    PRINT
    SYSTEM
 END IF

 LINE INPUT #1, CurDir$
 CurDir$ = RTRIM$(LTRIM$(UCASE$(CurDir$)))

 FOR i = 1 TO LEN(CurDir$)
     IF UCASE$(MID$(CurDir$, i, 12)) = UCASE$(DataFile$) THEN
	CurDir$ = UCASE$(LEFT$(CurDir$, i - 1))
	EXIT FOR
     END IF
 NEXT i
 IF RIGHT$(CurDir$, 1) <> "\" THEN CurDir$ = CurDir$ + "\"

 CLOSE #1
 KILL "CONFAIVE.TMP"
END SUB

SUB DrawBoard
 Button 0, 0, 319, 199, 0
 SELECT CASE GridMode
   CASE 0: FOR x = 9 TO 309 STEP 10
	       LINE (x, 9)-(x, 189), 8
	       IF x <= 189 THEN LINE (9, x)-(309, x), 8
	   NEXT x
   CASE 1: FOR x = 9 TO 309 STEP 10
	       LINE (x, 9)-(x, 189), 7, , &HAAAA
	       LINE (x, 9)-(x, 189), 8, , NOT &HAAAA
	       IF x <= 189 THEN
		  LINE (9, x)-(309, x), 7, , &HAAAA
		  LINE (9, x)-(309, x), 8, , NOT &HAAAA
	       END IF
	   NEXT x
   CASE 2: FOR x = 9 TO 299 STEP 10
	       FOR y = 9 TO 179 STEP 10
		   IF ((x + 1) / 10 + (y + 1) / 10) MOD 2 = 0 THEN c = 8 ELSE c = 0
		   LINE (x, y)-(x + 10, y + 10), c, BF
		   LINE (x, y)-(x + 10, y + 10), 7, B
	       NEXT y
	   NEXT x
   CASE 3: LINE (9, 9)-(309, 189), 0, BF
	   LINE (9, 9)-(309, 189), 8, B
 END SELECT
END SUB

REM $DYNAMIC
'DriversLoaded - Attempt to detect if sound drivers are loaded
SUB DriversLoaded (SBMIDI%, SBSIM%)
'Open the data file.
FF% = FREEFILE
OPEN "DRIVERS.DAT" FOR BINARY AS #FF%
FileSize& = LOF(FF%)
NoExist% = 0
'If the file is empty, return an error.
IF FileSize& = 0 THEN
	CLOSE FF%
	KILL "DRIVERS.DAT"
	MIDI.ERROR = 1
	NoExist% = 1
'If the file is not exactly 1,024 bytes in size, return an error.
ELSEIF FileSize& <> 1024 THEN
	CLOSE FF%
	MIDI.ERROR = 9
	NoExist% = 1
END IF

'If DRIVERS.DAT exists, and is 1 kilobyte in size, read the driver
'data from it.
IF NoExist% = 0 THEN
REDIM DRIVERDATA$(1 TO 5)
FOR i% = 1 TO 4
	DRIVERDATA$(i%) = INPUT$(256, #FF%)
NEXT i%
END IF

'Close the data file.
CLOSE #FF%

'Check the interrupt handlers for int 80h-FFh, to see if they are occupied
'by either SBMIDI or SBSIM.
SBMIDI% = 0
SBSIM% = 0
FOR i% = &H80 TO &HFF
	'Get the address of the interrupt handler.
	InternalGetIntVector i%, segment&, OFFSET&
	'If the segment returned is 0, that means that the current interrupt
	'is not in use.
	IF segment& = 0 THEN GOTO Skip:

	'The following code checks for the drivers by looking for the text
	'"SBMIDI" and "SBSIM" at certain locations in the driver code.
	'If it doesn't work, a different method is used.
	IF SBMIDI% = 0 THEN
	  DEF SEG = segment& - 17
	  TEMP$ = ""
	  FOR j% = 1 TO 6
		TEMP$ = TEMP$ + CHR$(PEEK(271 + j%))
	  NEXT
	  IF TEMP$ = "SBMIDI" THEN SBMIDI% = i%
	END IF
	IF SBSIM% = 0 AND segment& <> 0 THEN
		DEF SEG = segment& - 1
		TEMP$ = ""
		FOR j% = 1 TO 5
			TEMP$ = TEMP$ + CHR$(PEEK(274 + j%))
		NEXT
		IF TEMP$ = "SBSIM" THEN SBSIM% = i%
	END IF
 
	'This is the second detection method.  It's more complex than the first
	'method, but not really any more accurate.
	IF NoExist% = 0 THEN
	'Point to the segment of the interrupt handler.
	DEF SEG = segment&
	'Read 256 bytes of code from the interrupt handler.
	DRIVERDATA$(5) = ""
	FOR j% = 0 TO 255
		Byte% = PEEK(OFFSET& + j%)
		DRIVERDATA$(5) = DRIVERDATA$(5) + CHR$(Byte%)
	NEXT j%
	'Check to see if the code matches any of the data from DRIVERS.DAT.
	FOR j% = 1 TO 4
		MATCH% = 1
		FOR k% = 0 TO 255
			IF MID$(DRIVERDATA$(j%), k% + 1, 1) <> MID$(DRIVERDATA$(5), k% + 1, 1) THEN
				SELECT CASE k%
					CASE IS = 14, 15, 113, 114, 235, 236
					CASE ELSE
						MATCH% = 0
						EXIT FOR
				END SELECT
			END IF
		NEXT k%
		'If there was a match, find out which driver is using the interrupt.
		IF MATCH% THEN
			IF j% = 1 THEN SBSIM% = i%
			IF j% <> 1 THEN SBMIDI% = i%
		END IF
		'If both SBMIDI and SBSIM have been found, exit the loop.
		IF SBSIM% <> 0 AND SBMIDI% <> 0 THEN EXIT FOR
	NEXT j%
   
	'If both SBMIDI and SBSIM have been found, exit the loop.
	IF SBSIM% <> 0 AND SBMIDI% <> 0 THEN EXIT FOR
	END IF
Skip:
NEXT i%
IF NoExist% = 0 THEN MIDI.ERROR = 0
END SUB

REM $STATIC
FUNCTION Encode$ (Text$)  'by Dominik Kaspar

'Step [1]:
'Any character in the entered string will be changed into a 3-digit ASCII-
'code. This way, the string will become three times bigger.
'The following example string leads you through the explanation, so you will
'understand what happens, even if you don't understand the code.

'Example string: "A"  (this might be someone's password)

 t$ = ""
 FOR i = 1 TO LEN(Text$)
     ascii$ = LTRIM$(STR$(ASC(MID$(Text$, i, 1))))     'read ASCII code
     ascii$ = STRING$(3 - LEN(ascii$), "0") + ascii$   'fill in zeros
     t$ = t$ + ascii$
 NEXT i

'Example string: "065"  (Result after first step)

'STEP [2]:
'In the following paragraph, the string will become three times bigger again,
'because a random number from 04 to 24 will be put in front of every number.

 Text$ = t$: t$ = ""
 FOR i = 1 TO LEN(Text$)
     ascii$ = LTRIM$(STR$(INT(RND * 20) + 4))          'random number
     ascii$ = STRING$(2 - LEN(ascii$), "0") + ascii$   'fill in zeros
     t$ = t$ + ascii$ + MID$(Text$, i, 1)
 NEXT i

'                   .  .  .
'Example string: "130086235"  (the dots indicate the ASCII-code of 065,
'                 -- -- --     randomly filled in numbers are underlined)

'STEP [3]:
'At last, the program will go through the string again, reading three
'neighboring numbers to convert them into a symbol. This way, the string
'will become three times smaller again.

 Text$ = t$: t$ = ""
 FOR i = 1 TO LEN(Text$) STEP 3
     t$ = t$ + CHR$(VAL(MID$(Text$, i, 3)))
 NEXT i
 Encode$ = t$

'Example String: "‚Vë" (130 = ‚, 086 = V, 235 = ë (compare with above))

'IMPORTANT: It's very unlikely that if you enter "A" as a password "‚Vë" will
'           result! This makes it impossible to decipher an encoded string,
'           except you know the idea behind it... and this is very unlikely
'           as well...

END FUNCTION

SUB GameOver (p AS INTEGER)    'p is winning player
 Delay .5                      'delay of half a second

 Button 99, 59, 220, 130, 0    'drawing message box
 SELECT CASE PlayMode
   CASE 0:
	IF p = 1 THEN
	   PrintStr 109, 70, 12, "YOU WON."
	   Stat.Wins = Stat.Wins + 1
	  
	   IF Stat.StreakC < 0 THEN Stat.StreakC = 0
	   Stat.StreakC = Stat.StreakC + 1
	   IF Stat.StreakC > Stat.StreakW THEN Stat.StreakW = Stat.StreakC

	   IF Moves < Stat.Best OR Stat.Best = 0 THEN Stat.Best = Moves
	ELSEIF p = 2 THEN
	   Stat.Losses = Stat.Losses + 1
	   PrintStr 109, 70, 9, "YOU LOST."
	  
	   IF Stat.StreakC > 0 THEN Stat.StreakC = 0
	   Stat.StreakC = Stat.StreakC - 1
	   IF Stat.StreakC < Stat.StreakL THEN Stat.StreakL = Stat.StreakC

	   IF Moves < Stat.Worst OR Stat.Worst = 0 THEN Stat.Worst = Moves
	END IF
	FF% = FREEFILE
	OPEN CurDir$ + "STATS.CT5" FOR RANDOM AS FF% LEN = LEN(Stat)
	     PUT FF%, 1, Stat
	CLOSE FF%
   CASE 1, 2:
	IF p = 1 THEN
	   PrintStr 150, 70, P1C, P1S$
	ELSEIF p = 2 THEN
	   PrintStr 150, 70, P2C, P2S$
	END IF
	PrintStr 109, 70, 7, "Player"
	PrintStr 161, 70, 7, "WON!"
 END SELECT
 PrintStr 110, 85, 7, "Play again?"
 Button 109, 100, 155, 120, 7
 Button 165, 100, 210, 120, 7
 PrintStr 122, 107, 0, "YES"
 PrintStr 181, 107, 0, "NO"

 Moves = 0                     'reset moves

 Mouse 1
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8

     IF MouseK = 1 THEN
	WaitKeyUp
	IF x > 110 AND y > 100 AND x < 155 AND y < 120 THEN
	   Mouse 0
	   ButtonPressed 109, 100, 155, 120, 7
	   PrintStr 122, 107, 0, "YES"
	   Delay .5
	   CLS : Newgame
	   Mouse 1
	   EXIT SUB
	ELSEIF x > 165 AND y > 100 AND x < 210 AND y < 120 THEN
	   Mouse 0
	   ButtonPressed 165, 100, 210, 120, 7
	   PrintStr 181, 107, 0, "NO"
	   Delay .5
	   ReallyQuit
	END IF
     END IF
 LOOP
END SUB

SUB GetMouse
 R% = Interr%(&H33, 3, BX%, cx%, dx%)
 MouseK = BX%
 MouseX = cx% / 8 + 1
 MouseY = dx% / 8 + 1
END SUB

FUNCTION GetSynth%
QMIDIRegs.BX = 10
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
GetSynth% = QMIDIRegs.AX
END FUNCTION

SUB HelpScreen
 Mouse 0
 PCOPY 1, 0

 Button 0, 0, 319, 199, 0
 PrintStr 20, 20, 15, "CONFAIVE - GAME DESCRIPTION"
 PrintStr 20, 40, 7, "The game objective is to get a straight line"
 PrintStr 20, 55, 7, "of five connected symbols before your opponent"
 PrintStr 20, 70, 7, "does. Such lines can be made in horizontal"
 PrintStr 20, 85, 7, "vertical and diagonal direction."
 PrintStr 20, 100, 7, "In the options menu you can change to two"
 PrintStr 20, 115, 7, "player mode, and there are many more settings"
 PrintStr 20, 130, 7, "to manipulate. If you need help on that, then"
 PrintStr 20, 145, 7, "just try anything, or ask your mother."
 PrintStr 20, 165, 15, "HAVE FUN PLAYING CONFAIVE!"

 Mouse 1                        'show cursor of mouse
 DO
   WaitMove
 LOOP UNTIL MouseK
END SUB

SUB Hint
 Mouse 0         'hide mouse
 PCOPY 1, 0      'redraw game screen

 CalculateBest
 HintX = BestX * 10 - 1
 HintY = BestY * 10 - 1
 IF Turn = 1 THEN
    LINE (HintX, HintY)-(HintX + 10, HintY + 10), P1C, B
 ELSEIF Turn = 2 THEN
    LINE (HintX, HintY)-(HintX + 10, HintY + 10), P2C, B
 END IF
 FOR i = 1 TO 6
     IF Turn = 1 THEN
	IF i MOD 2 = 1 THEN
	   PrintStr HintX + 2, HintY + 2, P1C, P1S$
	ELSE
	   LINE (HintX + 1, HintY + 1)-(HintX + 9, HintY + 9), 0, BF
	END IF
     ELSEIF Turn = 2 THEN
	IF i MOD 2 = 1 THEN
	   PrintStr HintX + 2, HintY + 2, P2C, P2S$
	ELSE
	   LINE (HintX + 1, HintY + 1)-(HintX + 9, HintY + 9), 0, BF
	END IF
     END IF
     Delay .2
 NEXT i
 Mouse 1
END SUB

SUB InitializeFont
 DIM FontFileEntry AS INTEGER

 FF% = FREEFILE
 OPEN CurDir$ + "CONFAIVE.FNT" FOR RANDOM AS FF% LEN = LEN(FontFileEntry)
      rec = 0
      FOR i = 0 TO 127
	  FOR j = 1 TO 6
	      rec = rec + 1
	      GET FF%, rec, FontFileEntry
	      Font(i, j) = FontFileEntry
	  NEXT j
      NEXT i
 CLOSE FF%
END SUB

REM $DYNAMIC
SUB InternalGetIntVector (IntNum%, segment&, OFFSET&)
QMIDIRegs.AX = IntNum% + 13568
CALL IntX(&H21, QMIDIRegs)
segment& = QMIDIRegs.ES
OFFSET& = QMIDIRegs.BX
END SUB

REM $STATIC
FUNCTION Interr% (num%, AX%, BX%, cx%, dx%)
 IF MS%(0) = 0 THEN
    PRINT " ERROR: Could not load mouse!"
    END
 END IF
 DEF SEG = VARSEG(MS%(0))
 POKE VARPTR(MS%(0)) + 26, num%
 CALL ABSOLUTE(AX%, BX%, cx%, dx%, VARPTR(MS%(0)))
 Interr% = AX%
END FUNCTION

SUB Intro
 ShowBMP CurDir$ + "LOGO.CT5"
 Delay 2
 PrintStr 74, 180, 1, "Press a mouse key to start game"
 Mouse 1
 DO
   WaitMove
 LOOP UNTIL MouseK
 WaitKeyUp
 Mouse 0
END SUB

SUB IntX (IntNum AS INTEGER, Regs AS Registers) STATIC

STATIC filenum AS INTEGER, IntOffset AS INTEGER, Loaded AS INTEGER
		   
	' use fixed-length string to fix its position in memory
	' and so we don't mess up string pool before routine
	' gets its pointers from caller

DIM IntCode AS STRING * 200
IF NOT Loaded THEN                     ' loaded will be 0 first time
	RESTORE IntXCodeData:
   
	FOR k% = 1 TO 145
		READ h%
		MID$(IntCode, k%, 1) = CHR$(h%)
	NEXT

	'  determine address of interrupt no. offset in IntCode
  
	IntOffset% = INSTR(IntCode$, CHR$(&HCD) + CHR$(&H21)) + 1
	Loaded% = -1
END IF

SELECT CASE IntNum
  
	CASE &H25, &H26, IS > 255               ' ignore these interrupts
  
	CASE ELSE
		DEF SEG = VARSEG(IntCode)             ' poke interrupt number into
		POKE VARPTR(IntCode) * 1& + IntOffset - 1, IntNum     ' code block
		CALL ABSOLUTE(Regs, VARPTR(IntCode$))               ' call routine
END SELECT

END SUB

REM $DYNAMIC
'LoadMIDI - loads a MIDI file into memory
FUNCTION LoadMIDI% (FileName$)
LoadMIDI% = -1
'See if an extension was supplied, and if not, add one.
IF INSTR(FileName$, ".") = 0 THEN FileName$ = FileName$ + ".MID"
'Open the file
FF% = FREEFILE
OPEN FileName$ FOR BINARY AS #FF%
FileLen& = LOF(FF%)
CLOSE #FF%
'If the file is empty, delete it and exit now.
IF FileLen& = 0 THEN KILL FileName$: MIDI.ERROR = 1: EXIT FUNCTION
'Make the filename an ASCIIZ string.
FileName$ = FileName$ + CHR$(0)

'Find an empty MIDI handle
NewHandle% = -1
FOR i% = 0 TO 255
	IF MEM.SEGMENT(i%) = 0 THEN NewHandle% = i%: EXIT FOR
NEXT i%
'If there are no empty handles, return an error.
IF NewHandle% = -1 THEN MIDI.ERROR = 12: EXIT FUNCTION
'Attempt to allocate a block of conventional memory.
QMIDIRegs.AX = &H4800
QMIDIRegs.BX = (FileLen& \ 16) + 1
CALL IntX(&H21, QMIDIRegs)
'If the block couldn't be allocated, it means there's not enough free
'memory.  To fix this, we need to ask BASIC to release some of the memory
'it's using:
IF QMIDIRegs.AX = 7 OR QMIDIRegs.AX = 8 THEN
	'Find out how much memory is available, in kilobytes.
	LargestBlock& = QMIDIRegs.BX
	LargestBlock& = LargestBlock& * 16
	'Calculate the amount of memory that BASIC needs to release for us.
	MEM.ALLOCATED(NewHandle%) = (FileLen& + 2048) - LargestBlock&
	'Attempt to release the memory.
	A& = SETMEM(-MEM.ALLOCATED(NewHandle%))
	'Try again to allocate a block of memory
	QMIDIRegs.AX = &H4800
	QMIDIRegs.BX = (FileLen& \ 16) + 1
	CALL IntX(&H21, QMIDIRegs)
	'If the second attempt was unsuccessful, then there just isn't
	'enough memory, and an error needs to be returned.
	IF QMIDIRegs.AX = 7 OR QMIDIRegs.AX = 8 THEN
		'Give any memory we took back to BASIC.
		A& = SETMEM(650000)
		'Return an error.
		MIDI.ERROR = 2
		MEM.SEGMENT(NewHandle%) = 0
		'Abort.
		EXIT FUNCTION
	END IF
END IF
'If the memory was allocated successfully, store the segment
'of the memory block.
MEM.SEGMENT(NewHandle%) = QMIDIRegs.AX
MIDISegment& = QMIDIRegs.AX

'Open the MIDI file using a DOS interrupt.
QMIDIRegs.AX = &H3D00
QMIDIRegs.dx = SADD(FileName$)
QMIDIRegs.DS = VARSEG(FileName$)
CALL IntX(&H21, QMIDIRegs)
'Store the file handle.
Handle% = QMIDIRegs.AX
'Read the data from the file in 16 kilobyte increments.
FOR i& = 1 TO FileLen& STEP 16384
	QMIDIRegs.AX = &H3F00
	QMIDIRegs.cx = 16384
	QMIDIRegs.dx = 0
	QMIDIRegs.DS = VAL("&H" + HEX$(MIDISegment&))
	QMIDIRegs.BX = Handle%
	CALL IntX(&H21, QMIDIRegs)
	MIDISegment& = MIDISegment& + 1024
NEXT i&

'Close the file
QMIDIRegs.AX = &H3E00
QMIDIRegs.BX = Handle%
CALL IntX(&H21, QMIDIRegs)

MIDI.ERROR = 0
LoadMIDI% = NewHandle%
END FUNCTION

REM $STATIC
SUB LoopMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
QMIDIRegs.BX = 11
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
IF QMIDIRegs.AX = 0 THEN PlayMidi CURRENTHANDLE
END SUB

FUNCTION MemUsed& (Handle%)
MemUsed& = MEM.ALLOCATED(Handle%)
MIDI.ERROR = 0
END FUNCTION

SUB MenuBar
 Mouse 0                        'hiding mouse
 PCOPY 0, 1                     'saving screen

 Button 0, 0, 319, 16, 0        'drawing menu bar
 Button 0, 184, 319, 199, 0     '   "    description bar

 inew = 0: iold = 0             'variables for managing highlighted items
 iinew = 0: iiold = 0

 Mouse 1                        'show cursor of mouse
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
    
     IF y < 17 THEN             'only display pull-down menu when cursor is
	inew = INT(x / 80) + 1  'over menu bar.
     END IF
     IF inew > 4 THEN inew = 4 ELSE IF inew < 1 THEN inew = 1
     IF inew <> iold THEN
	Mouse 0
	IF iold <> 0 THEN PCOPY 2, 0
	FOR j = 1 TO 4          'highlighting menu bar itmes
	    IF inew = j THEN c = 15 ELSE c = 7
	    PrintStr 5 + 80 * (j - 1), 5, c, mnuItem(j, 1)
	NEXT j
	IF iold = 0 THEN PCOPY 0, 2
	iold = inew
	cnt = 0
	FOR j = 2 TO 9          'counting items in pull-down menu
	    IF INSTR(1, mnuItem(inew, j), "#") = 0 THEN cnt = cnt + 1
	NEXT j
	cx = 80 * (inew - 1): cy = 17
	dx = 80 * inew - 1: dy = 17 + cnt * 15
	Button cx, cy, dx, dy, 0
	FOR j = 2 TO 1 + cnt
	    c = 7
	    PrintStr 5 + cx, 22 + (j - 2) * 15, c, mnuItem(inew, j)
	NEXT j
	LINE (5, 189)-(310, 195), 0, BF
	PrintStr 5, 189, 7, hlpItem(inew, 1)
	Mouse 1
     END IF

     IF x > cx AND y > cy AND x < dx AND y < dy THEN
	iinew = INT((y - 22) / 15) + 2
	IF iinew <> iiold THEN
	   Mouse 0
	   FOR j = 2 TO cnt + 1
	       IF iinew = j THEN c = 15 ELSE c = 7
	       PrintStr 5 + cx, 22 + (j - 2) * 15, c, mnuItem(iold, j)
	   NEXT j
	   iiold = iinew
	   LINE (5, 189)-(310, 195), 0, BF
	   PrintStr 5, 189, 7, hlpItem(iold, iiold)
	   Mouse 1
	END IF
     END IF

     IF MouseK = 1 THEN
	WaitKeyUp
	IF x > cx AND y > cy AND x < dx AND y < dy THEN
	   SELECT CASE iold
	     CASE 1: SELECT CASE (iiold - 1)
			    CASE 1: StoppedGame
				    Mouse 0
				    CLS : Newgame
				    Mouse 1
				    EXIT SUB
			    CASE 2: ReallyQuit
		     END SELECT
	     CASE 2: SELECT CASE (iiold - 1)
			    CASE 1: Undo
			    CASE 2: Hint
			    CASE 3: ChangeAI
			    CASE 4: ChangePlayers
			    CASE 5: ChangeGrid
			    CASE 6: ChangeMusic
			    CASE 7: SetDefault
		     END SELECT
	     CASE 3: SELECT CASE (iiold - 1)
			    CASE 1: StatShow
			    CASE 2: StatReset
			    CASE 3: WhoIsWinner
		     END SELECT
	     CASE 4: SELECT CASE (iiold - 1)
			    CASE 1: HelpScreen
			    CASE 2: About
		     END SELECT
	   END SELECT
	END IF
	Mouse 0                 'hide mouse
	PCOPY 1, 0              'paste old screen
	Mouse 1                 'show mouse again
	EXIT SUB                'go back to game screen
     END IF
 LOOP
END SUB

REM $DYNAMIC
'MIDIError - Translates a QMIDI error code into text
FUNCTION MIDIError$
SELECT CASE MIDI.ERROR
		CASE 0: MIDIError$ = "NO ERROR"
		CASE 1: MIDIError$ = "FILE DOES NOT EXIST"
		CASE 2: MIDIError$ = "OUT OF MEMORY"
		CASE 3: MIDIError$ = "NO MIDI FILE PLAYING"
		CASE 4: MIDIError$ = "INVALID SBMIDI INTERRUPT"
		CASE 5: MIDIError$ = "INVALID SBSIM INTERRUPT"
		CASE 6: MIDIError$ = "NO MIXER CHIP"
		CASE 7: MIDIError$ = "COULD NOT DETECT SOUND CARD"
		CASE 8: MIDIError$ = "FEATURE UNAVAILABLE"
		CASE 9: MIDIError$ = "FILE IS CORRUPT"
		CASE 10: MIDIError$ = "INVALID SOUND CARD TYPE"
		CASE 11: MIDIError$ = "COULD NOT PLAY MUSIC"
		CASE 12: MIDIError$ = "ALL HANDLES IN USE"
		CASE 13: MIDIError$ = "INVALID HANDLE NUMBER"
		CASE ELSE: MIDIError$ = "UNKNOWN ERROR"
END SELECT
END FUNCTION

REM $STATIC
SUB Mouse (OnOff%)
 IF OnOff% = 0 THEN OnOff% = 2 ELSE OnOff% = 1
 R% = Interr%(&H33, OnOff%, BX%, cx%, dx%)
END SUB

SUB MouseInit
 R% = Interr%(&H33, 0, BX%, cx%, dx%)
 R% = Interr%(&H33, 10, 0, &HFFFF, &HFF00)
END SUB

SUB MouseSet (x%, y%)
 R% = Interr%(&H33, 4, BX%, x% * 8 - 8, y% * 8 - 8)
END SUB

SUB MouseXLim (x1%, x2%)
 R% = Interr%(&H33, 7, 0, x1% * 2 - 2, x2% * 2 - 2)
END SUB

SUB MouseYLim (y1%, y2%)
 R% = Interr%(&H33, 8, BX%, y1% - 2, y2% - 2)
END SUB

FUNCTION MusicDone%
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT FUNCTION
IF MIDI.PLAYTIME = 0 THEN MIDI.ERROR = 3: EXIT FUNCTION
QMIDIRegs.BX = 11
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
IF QMIDIRegs.AX = 0 THEN QMIDIRegs.AX = -1 ELSE QMIDIRegs.AX = 0
MusicDone% = QMIDIRegs.AX
MIDI.ERROR = 0
END FUNCTION

SUB Newgame
 ClearBoard     'Empty the board
 DrawBoard      'Draw the emptied board to the screen

 Moves = 0      'Reset move counter for new game
 BestX = 0      'Reset variables that hold best square's position
 BestY = 0
 Turn = 1

 FOR i = 1 TO 540          'Clear undo-list
     MoveList(i, 1) = 0
     MoveList(i, 2) = 0
 NEXT i
END SUB

REM $DYNAMIC
'PauseMIDI - Pauses a MIDI file that is currently playing
SUB PauseMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'If no MIDI file is playing, exit now
IF MIDI.PLAYTIME = 0 THEN
	MIDI.ERROR = 3
	EXIT SUB
END IF
'Call the SBSIM driver to pause the music.
QMIDIRegs.BX = 7
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
'Save the number of seconds that the MIDI file has been playing.
PAUSED = TimeMIDI!
'If the music hasn't been playing long enough for TimeMIDI! to return
'a value greater than 0, change PAUSED to a tiny positive value.
IF PAUSED = 0! THEN PAUSED = .00001
'Indicate that the file has stopped playing.
MIDI.PLAYTIME = 0
MIDI.ERROR = 0
END SUB

REM $STATIC
SUB PlayGame
 IF Turn = 2 AND PlayMode = 0 THEN GOTO ComputerTurn
 Mouse 1                 'Show cursor of mouse
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
 
     IF y <= 3 THEN      'If mouse cursor is at top of screen, then
	MenuBar          'show menu bar
     END IF
   
     IF MouseK = 1 THEN
	WaitKeyUp
	BestX = INT(x / 10)
	BestY = INT(y / 10)
	SELECT CASE PlayMode
	 CASE 0:                   '[0]=Human/Computer
	  IF Board(BestX, BestY) = 0 OR Board(BestX, BestY) = 8 THEN
	     Mouse 0
	     PrintStr BestX * 10 + 1, BestY * 10 + 1, P1C, P1S$
	     Board(BestX, BestY) = 1
	     IF WinCheck(Turn) = 1 THEN
		GameOver Turn
	     ELSE
		ClearAround BestX, BestY, 1
		Turn = 2
		Moves = Moves + 1
		MoveList(Moves, 1) = BestX
		MoveList(Moves, 2) = BestY
ComputerTurn:
		CalculateBest
		PrintStr BestX * 10 + 1, BestY * 10 + 1, P2C, P2S$
		Board(BestX, BestY) = 2
		Moves = Moves + 1
		MoveList(Moves, 1) = BestX
		MoveList(Moves, 2) = BestY
		IF WinCheck(Turn) = 1 THEN
		   GameOver Turn
		ELSE
		   Turn = 1
		END IF
		Mouse 1
	     END IF
	  END IF
	 CASE 2:                   '[2]=Human/Human
	  IF Board(BestX, BestY) = 0 OR Board(BestX, BestY) = 8 THEN
	     Mouse 0
	     IF Turn = 1 THEN
		PrintStr BestX * 10 + 1, BestY * 10 + 1, P1C, P1S$
	     ELSE
		PrintStr BestX * 10 + 1, BestY * 10 + 1, P2C, P2S$
	     END IF
	     Board(BestX, BestY) = Turn
	     IF WinCheck(Turn) = 1 THEN
		GameOver Turn
	     ELSE
		ClearAround BestX, BestY, 1
		IF Turn = 1 THEN Turn = 2 ELSE Turn = 1
		Moves = Moves + 1
		MoveList(Moves, 1) = BestX
		MoveList(Moves, 2) = BestY
	     END IF
	     Mouse 1
	  END IF
	END SELECT
     END IF

     IF PlayMode = 1 THEN          '[1]=Computer/Computer
	IF CalculatingBest = 0 THEN
	   CalculatingBest = 1
	   CalculateBest
	   CalculatingBest = 0
	   Mouse 0
	   IF Turn = 1 THEN
	      PrintStr BestX * 10 + 1, BestY * 10 + 1, P1C, P1S$
	   ELSE
	      PrintStr BestX * 10 + 1, BestY * 10 + 1, P2C, P2S$
	   END IF
	   Board(BestX, BestY) = Turn
	   IF Turn = 1 THEN Turn = 2 ELSE Turn = 1
	   Moves = Moves + 1
	   MoveList(Moves, 1) = BestX
	   MoveList(Moves, 2) = BestY
	   IF WinCheck(Turn) = 1 THEN GameOver Turn
	   Mouse 1
	END IF
     END IF
 LOOP
END SUB

REM $DYNAMIC
'PlayMIDI - Begins playing a MIDI file in the background.
SUB PlayMidi (Handle%)
IF Handle% < 0 OR Handle% > 255 THEN MIDI.ERROR = 13: EXIT SUB
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'If sound is not disabled....
IF SOUND.DISABLED = 0 THEN
	'Call the SBMIDI driver to begin playing the MIDI file.
	QMIDIRegs.BX = 4
	QMIDIRegs.dx = MEM.SEGMENT(Handle%)
	QMIDIRegs.AX = 0
	CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
	QMIDIRegs.BX = 5
	CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
	'If the music could not be started, return an error.
	IF QMIDIRegs.AX <> 0 THEN MIDI.ERROR = 11: EXIT SUB
	'Start the MIDI timer.
	MIDI.PLAYTIME = TIMER
	'Set the current handle.
	CURRENTHANDLE = Handle%
END IF
MIDI.ERROR = 0
END SUB

REM $STATIC
SUB PrintStr (XPos, YPos, Colour, Text$)
 x = XPos
 y = YPos
 c = Colour
 FOR j = 1 TO LEN(Text$)
     rec = ASC(MID$(Text$, j, 1))
     FOR i = 0 TO 5
	 bin = Font(rec, i + 1)
	 LINE (x - 6 + j * 6, y + i)-(x + j * 6, y + i), c, , bin
     NEXT i
 NEXT j
END SUB

FUNCTION Prompt$ (XPos, YPos, c, Text$, Limit)

 PrintStr XPos, YPos, c, Text$

 Answer$ = ""

 DO
   LINE (XPos, YPos)-(XPos + Limit * 6, YPos + 5), 0, BF
   PrintStr XPos + LEN(Text$) * 6, YPos, c, Answer$
  
   Key$ = ""
   DO: Key$ = INKEY$: LOOP UNTIL LEN(Key$)
   
   SELECT CASE ASC(Key$)
     CASE 8:
	  IF LEN(Answer$) >= 1 THEN
	     Answer$ = LEFT$(Answer$, LEN(Answer$) - 1)
	  END IF
     CASE 13:
	  EXIT DO
     CASE ELSE:
	  IF LEN(Answer$) < Limit THEN
	     IF ASC(Key$) >= 32 THEN Answer$ = Answer$ + Key$
	  END IF
   END SELECT
 LOOP

 Prompt$ = Answer$

END FUNCTION

SUB ReadData
 RESTORE MS.Data
 DEF SEG = VARSEG(MS%(0))
 FOR i% = 0 TO 99
     READ Byte$
     IF Byte$ = "#" THEN EXIT FOR
     POKE VARPTR(MS%(0)) + i%, VAL("&H" + Byte$)
 NEXT i%
END SUB

SUB ReadMenuBar
 DIM Text AS STRING * 30
 DIM Dscr AS STRING * 150
 F1% = FREEFILE
 OPEN CurDir$ + "MENUBAR1.CT5" FOR RANDOM AS F1% LEN = LEN(Text)
 F2% = FREEFILE
 OPEN CurDir$ + "MENUBAR2.CT5" FOR RANDOM AS F2% LEN = LEN(Dscr)
 rec = 0
 FOR i = 1 TO 4
     FOR j = 1 TO 9
	 rec = rec + 1
	 GET F1%, rec, Text
	 GET F2%, rec, Dscr
	 mnuItem(i, j) = Decode(Text)
	 hlpItem(i, j) = Decode(Dscr)
     NEXT j
 NEXT i
 CLOSE F1%, F2%
END SUB

SUB ReadMoveList
 FF% = FREEFILE
 OPEN CurDir$ + "MOVELIST.CT5" FOR RANDOM AS FF% LEN = LEN(MoveListFile)
      IF LOF(FF%) = 0 THEN
	 CLOSE FF%
	 KILL CurDir$ + "MOVELIST.CT5"
	 EXIT SUB
      END IF
      FOR i = 1 TO 540
	  GET FF%, i, MoveListFile
	  MoveList(i, 1) = MoveListFile.XElement
	  MoveList(i, 2) = MoveListFile.YElement
	  IF MoveList(i, 1) THEN
	     IF i MOD 2 = 1 THEN
		Board(MoveList(i, 1), MoveList(i, 2)) = 1
		c = P1C: p$ = P1S$
	     ELSE
		Board(MoveList(i, 1), MoveList(i, 2)) = 2
		c = P2C: p$ = P2S$
	     END IF
	     ClearAround MoveList(i, 1), MoveList(i, 2), 1
	     PrintStr MoveList(i, 1) * 10 + 1, MoveList(i, 2) * 10 + 1, c, p$
	  END IF
      NEXT i
 CLOSE FF%
 KILL CurDir$ + "MOVELIST.CT5"
END SUB

SUB ReadOptions
 FF% = FREEFILE
 OPEN CurDir$ + "OPTIONS.CT5" FOR RANDOM AS FF% LEN = LEN(Opt)
   IF LOF(FF%) = 0 THEN
      Opt.oP1C = 12
      Opt.oP1S = "X"
      Opt.oP2C = 9
      Opt.oP2S = "O"
      Opt.oMid1 = 1
      Opt.oMid2 = 1
      Opt.oMid3 = 1
      Opt.oMid4 = 1
      Opt.oMid5 = 1
      Opt.oMid6 = 1
      Opt.oMidiFile = 1
      Opt.oPlayMode = 0
      Opt.oStrategy = 2
      Opt.oRandomIQ = 100
      Opt.oDensCalc = 1
      Opt.oGridMode = 1
      Opt.oTurn = 1
      Opt.oMoves = 0
      PUT FF%, 1, Opt
   END IF
   GET FF%, 1, Opt
   P1C = Opt.oP1C
   P1S$ = Opt.oP1S
   P2C = Opt.oP2C
   P2S$ = Opt.oP2S
   MidiChk(0) = 0
   MidiChk(1) = Opt.oMid1
   MidiChk(2) = Opt.oMid2
   MidiChk(3) = Opt.oMid3
   MidiChk(4) = Opt.oMid4
   MidiChk(5) = Opt.oMid5
   MidiChk(6) = Opt.oMid6
   MidiFile = Opt.oMidiFile
   PlayMode = Opt.oPlayMode
   Strategy = Opt.oStrategy
   RandomIQ = Opt.oRandomIQ
   DensCalc = Opt.oDensCalc
   GridMode = Opt.oGridMode
   Turn = Opt.oTurn
   Moves = Opt.oMoves
 CLOSE FF%
END SUB

SUB ReadStats
 FF% = FREEFILE
 OPEN CurDir$ + "STATS.CT5" FOR RANDOM AS FF% LEN = LEN(Stat)
      IF LOF(FF%) = 0 THEN
	 Stat.Wins = 0
	 Stat.Losses = 0
	 Stat.StreakW = 0
	 Stat.StreakL = 0
	 Stat.StreakC = 0
	 Stat.Best = 0
	 Stat.Worst = 0
	 Stat.Stopped = 0
	 PUT FF%, 1, Stat
      END IF
      GET FF%, 1, Stat
 CLOSE FF%
END SUB

SUB ReallyQuit
 WaitKeyUp

 IF Moves THEN
    Mouse 0

    Button 99, 59, 220, 130, 0         'drawing message box
    PrintStr 109, 70, 7, "Save current game"
    PrintStr 110, 85, 7, "for next time?"
    Button 109, 100, 155, 120, 7
    Button 165, 100, 210, 120, 7
    PrintStr 122, 107, 0, "YES"
    PrintStr 181, 107, 0, "NO"

    Mouse 1                            'show cursor of mouse
    DO: WaitMove
	x = MouseX * 4 - 4
	y = MouseY * 8 - 8
	IF MouseK = 1 THEN
	   IF x > 109 AND y > 100 AND x < 155 AND y < 120 THEN
	      Mouse 0

	      FF% = FREEFILE
	      OPEN CurDir$ + "MOVELIST.CT5" FOR RANDOM AS FF% LEN = LEN(MoveListFile)
	      FOR i = 1 TO 540
		  MoveListFile.XElement = MoveList(i, 1)
		  MoveListFile.YElement = MoveList(i, 2)
		  PUT FF%, i, MoveListFile
	      NEXT i
	      CLOSE FF%

	      ButtonPressed 109, 100, 155, 120, 7
	      PrintStr 122, 107, 0, "YES"
	      Delay .5
	      Button 109, 100, 155, 120, 7
	      PrintStr 122, 107, 0, "YES"
	      EXIT DO
	   END IF
	   IF x > 165 AND y > 100 AND x < 210 AND y < 120 THEN
	      Mouse 0
	      ButtonPressed 165, 100, 210, 120, 7
	      PrintStr 181, 107, 0, "NO"
	      Delay .5
	      Button 165, 100, 210, 120, 7
	      PrintStr 181, 107, 0, "NO"
	      StoppedGame
	      EXIT DO
	   END IF
	END IF
    LOOP
 END IF
		  'saving options:
 FF% = FREEFILE
 OPEN CurDir$ + "OPTIONS.CT5" FOR RANDOM AS FF% LEN = LEN(Opt)
   Opt.oP1C = P1C
   Opt.oP1S = P1S$
   Opt.oP2C = P2C
   Opt.oP2S = P2S$
   Opt.oMid1 = MidiChk(1)
   Opt.oMid2 = MidiChk(2)
   Opt.oMid3 = MidiChk(3)
   Opt.oMid4 = MidiChk(4)
   Opt.oMid5 = MidiChk(5)
   Opt.oMid6 = MidiChk(6)
   Opt.oMidiFile = MidiFile
   Opt.oPlayMode = PlayMode
   Opt.oStrategy = Strategy
   Opt.oRandomIQ = RandomIQ
   Opt.oDensCalc = DensCalc
   Opt.oGridMode = GridMode
   Opt.oTurn = Turn
   Opt.oMoves = Moves
   PUT FF%, 1, Opt
 CLOSE FF%

 StopMidi                   'stop & unload any midi file playing
 UnloadMidi MidiHandle
 CleanUpMidi

 CLS
 ShowBMP CurDir$ + "THANKS.CT5"

 DO
   WaitMove
 LOOP UNTIL MouseK OR LEN(INKEY$)
 SYSTEM
END SUB

REM $DYNAMIC
'ResumeMIDI - Starts playing a MIDI file after it has been paused
SUB ResumeMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'If the MIDI file is not paused, exit now
IF PAUSED = 0! THEN EXIT SUB
'Call the SBSIM driver to resume playing.
QMIDIRegs.BX = 8
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
'Update the MIDI timer.
MIDI.PLAYTIME = TIMER - PAUSED
PAUSED = 0!
MIDI.ERROR = 0
END SUB

REM $STATIC
SUB SetDefault
 FF% = FREEFILE
 OPEN CurDir$ + "OPTIONS.CT5" FOR RANDOM AS FF% LEN = LEN(Opt)
 CLOSE FF%
 KILL CurDir$ + "OPTIONS.CT5"  'resetting data step one
 i1 = Moves
 i2 = Turn                     'saving still needed data
 i3 = MidiFile
 ReadOptions                   'resetting data step two
 Moves = i1
 Turn = i2                     'restoring still needed data
 MidiFile = i3
 UpdateScreen                  'drawing new look of screen
END SUB

SUB ShowBMP (data$)
DEFINT A-Z
DIM Byte AS STRING * 1

IF LTRIM$(RTRIM$(data$)) = "" THEN END
OPEN data$ FOR BINARY AS #1
IF LOF(1) = 0 THEN PRINT "File (" + data$ + ") not found!": CLOSE : KILL data$: END

table$ = INPUT$(54, #1)         'Get the file header (54 bytes)
DIM table&(30)                  'Create numerical array for header
DEF SEG = VARSEG(table&(1))
pointer% = VARPTR(table&(1))

'Poke the data from string "table$" into numerical array "table&"
FOR x% = 0 TO 51
 POKE pointer% + x%, ASC(MID$(table$, x% + 3, 1))
NEXT
DEF SEG

'Check for valid file type
IF MID$(table$, 1, 2) <> "BM" OR table&(4) <> 40 THEN
   PRINT "Not a valid *.BMP file!": END
END IF
IF table&(8) <> 0 THEN
   PRINT "This program will not diplay RLE encoded files": END
END IF

'Set the video mode for best picture fit
IF (table&(5) < 321) AND (table&(6) < 201) THEN
   SCREEN 13
ELSE
   SCREEN 12
END IF
LOCATE 2, 1
'PRINT " Image is "; table&(5); " by "; table&(6)

thecolors$ = INPUT$(table&(3) - 54, #1) 'Read in pallette info

DEF SEG = VARSEG(pal&)
pointer% = VARPTR(pal&)
'Poke the pallette info from the string "thecolors$"
'into pal& and reduce to 6 bits per byte.
FOR x% = 0 TO 63 STEP 4
 POKE pointer%, (ASC(MID$(thecolors$, x% + 3, 1))) \ 4
 POKE pointer% + 1, (ASC(MID$(thecolors$, x% + 2, 1))) \ 4
 POKE pointer% + 2, (ASC(MID$(thecolors$, x% + 1, 1))) \ 4
 POKE pointer% + 3, 0
PALETTE x% \ 4, pal&
NEXT
DEF SEG


'Read in Bitmap data and set pixels accordingly
y% = table&(6) 'Put number of vertical pixels into y%
DO
  data$ = INPUT$((((table&(5) - 1) OR 7) + 1) \ 2, #1)
    IF (table&(5) \ 2) < LEN(data$) THEN
       linelength% = table&(5) \ 2
    ELSE
       linelength% = LEN(data$)
    END IF
    FOR x% = 1 TO linelength%
      pixel% = ASC(MID$(data$, x%, 1))
      PSET (x% * 2 + 1, y%), pixel% AND 15
      PSET (x% * 2, y%), pixel% \ 16
     NEXT
    y% = y% - 1
LOOP UNTIL EOF(1)
CLOSE #1
END SUB

DEFSNG A-Z
SUB StatReset
 FF% = FREEFILE
 OPEN CurDir$ + "STATS.CT5" FOR RANDOM AS FF% LEN = LEN(Stat)
      Stat.Wins = 0
      Stat.Losses = 0
      Stat.StreakW = 0
      Stat.StreakL = 0
      Stat.StreakC = 0
      Stat.Best = 0
      Stat.Worst = 0
      Stat.Stopped = 0
      PUT FF%, 1, Stat
      GET FF%, 1, Stat
 CLOSE FF%

 Mouse 0
 PCOPY 1, 0

 Button 64, 85, 255, 115, 0                     '
 PrintStr 77, 97, 7, "Statistical Data is Cleared."

 Mouse 1                        'show cursor of mouse
 DO: WaitMove
     IF MouseK = 1 THEN EXIT SUB
 LOOP
END SUB

SUB StatShow
 Mouse 0
 PCOPY 1, 0

 Button 59, 19, 260, 170, 0
 IF Stat.Wins > Stat.Losses THEN i = LEN(STR$(Stat.Wins)) ELSE i = LEN(STR$(Stat.Losses))
 IF Stat.Wins + Stat.Losses = 0 THEN
    p = .5
 ELSE
    p = Stat.Wins / (Stat.Wins + Stat.Losses)
 END IF
 p100 = 115 - (i - 1) * 6
 LINE (245 - p100, 45)-(245 - p100 + p * p100, 50), P1C, BF
 LINE (245 - p100, 55)-(245 - p100 + (1 - p) * p100, 60), P2C, BF
 PrintStr 75, 30, 9, "STATISTICS  (Human/Computer)"
 PrintStr 75, 45, 7, "Wins:  " + STR$(Stat.Wins)
 PrintStr 75, 55, 7, "Losses:" + STR$(Stat.Losses)
 PrintStr 75, 65, 7, "Unfinished games:" + STR$(Stat.Stopped)
 A$ = STR$(ABS(Stat.StreakL))
 IF Stat.StreakC < 0 THEN B$ = " Losses" ELSE B$ = " Wins"
 IF Stat.StreakC = -1 THEN B$ = " Loss"
 IF Stat.StreakC = 1 THEN B$ = " Win"
 B$ = STR$(ABS(Stat.StreakC)) + B$
 PrintStr 75, 80, 7, "Streaks: Wins:   " + STR$(Stat.StreakW)
 PrintStr 75, 90, 7, "         Losses: " + A$
 PrintStr 75, 100, 7, "         Current:" + B$
 IF Stat.Best > 0 THEN
    A$ = "Won in" + STR$(INT(Stat.Best / 2) + 1) + " moves"
 ELSE
    A$ = "No entry found"
 END IF
 IF Stat.Worst > 0 THEN
    B$ = "Lost in" + STR$(INT(Stat.Worst / 2)) + " moves"
 ELSE
    B$ = "No entry found"
 END IF
 PrintStr 75, 115, 7, "Best:  " + A$
 PrintStr 75, 125, 7, "Worst: " + B$

 Button 140, 142, 180, 162, 7
 PrintStr 153, 149, 0, "OK"

 Mouse 1                        'show mouse cursor
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
     IF MouseK = 1 THEN
	WaitKeyUp
	Mouse 0
	IF x > 140 AND y > 142 AND x < 180 AND y < 162 THEN
	   ButtonPressed 140, 142, 180, 162, 7
	   PrintStr 153, 149, 0, "OK"
	   Delay .5
	   EXIT DO
	END IF
	Mouse 1
     END IF
 LOOP
 Mouse 1
END SUB

REM $DYNAMIC
'StopMIDI - Stops playing MIDI file
SUB StopMidi
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'Call the SBMIDI driver to stop the music.
IF MIDI.PLAYTIME THEN
	QMIDIRegs.BX = 4
	QMIDIRegs.dx = MEM.SEGMENT(CURRENTHANDLE)
	QMIDIRegs.AX = 0
	CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
	MIDI.ERROR = 0
ELSE
	MIDI.ERROR = 3
END IF
MIDI.PLAYTIME = 0
END SUB

REM $STATIC
SUB StoppedGame
 IF Moves > 0 AND PlayMode = 0 THEN
    FF% = FREEFILE
    OPEN CurDir$ + "STATS.CT5" FOR RANDOM AS FF% LEN = LEN(Stat)
	 Stat.Stopped = Stat.Stopped + 1
	 PUT FF%, 1, Stat
    CLOSE FF%
 END IF
END SUB

REM $DYNAMIC
FUNCTION TimeMIDI!
'If a MIDI file is paused, lock the current playing time
IF PAUSED > 0! THEN
	TimeMIDI! = PAUSED
	MIDI.ERROR = 0
'If a MIDI file is playing, carry out the timing routine
ELSEIF MIDI.PLAYTIME THEN
	'Get the current time
	CurrentTime! = TIMER
	'If midnight has come since the MIDI file started playing, change
	'CurrentTime! accordingly
	IF CurrentTime! - MIDI.PLAYTIME < 0 THEN
		CurrentTime! = 86400 + CurrentTime!
	END IF
	'Get the final result
	TimeMIDI! = CurrentTime! - MIDI.PLAYTIME
	MIDI.ERROR = 0
ELSE
	MIDI.ERROR = 3
END IF
END FUNCTION

REM $STATIC
SUB Undo
 IF Moves < 2 THEN EXIT SUB
 
 Mouse 0
 PCOPY 1, 0

 SELECT CASE PlayMode      '[0]=H/C  [1]=C/C  [2]=H/H
   CASE 0: FOR i = 0 TO 1
	       x = MoveList(Moves - i, 1)
	       y = MoveList(Moves - i, 2)
	       MoveList(Moves - i, 1) = 0
	       MoveList(Moves - i, 2) = 0
	       Board(x, y) = 0
	       IF GridMode = 2 THEN
		  IF (x + y) MOD 2 = 0 THEN c = 8 ELSE c = 0
	       END IF
	       LINE (x * 10, y * 10)-(x * 10 + 8, y * 10 + 8), c, BF
	   NEXT i
	   Moves = Moves - 2
   CASE 1:
   CASE 2: x = MoveList(Moves, 1)
	   y = MoveList(Moves, 2)
	   MoveList(Moves, 1) = 0
	   MoveList(Moves, 2) = 0
	   Board(x, y) = 0
	   IF GridMode = 2 THEN
	      IF (x + y) MOD 2 = 0 THEN c = 8 ELSE c = 0
	   END IF
	   LINE (x * 10, y * 10)-(x * 10 + 8, y * 10 + 8), c, BF
	   IF Turn = 1 THEN Turn = 2 ELSE Turn = 1
	   Moves = Moves - 1
 END SELECT

 PCOPY 0, 1
 Mouse 1
END SUB

SUB UnloadMidi (Handle%)
IF Handle% < 0 OR Handle% > 255 THEN MIDI.ERROR = 13: EXIT SUB
'If a block of memory was allocated to hold the MIDI file....
IF MEM.SEGMENT(Handle%) THEN
	'Release the block of memory.
	QMIDIRegs.ES = MEM.SEGMENT(Handle%)
	QMIDIRegs.AX = &H4900
	CALL IntX(&H21, QMIDIRegs)
	'Give back all the memory we took from BASIC.
	A& = SETMEM(650000)
END IF
MEM.SEGMENT(Handle%) = 0
MEM.ALLOCATED(Handle%) = 0
MIDI.ERROR = 0
END SUB

SUB UpdateScreen
 PCOPY 1, 0
 CLS
 DrawBoard
 FOR i = 1 TO 540
     IF MoveList(i, 1) = 0 THEN
	EXIT FOR
     ELSE
	IF i MOD 2 = 1 THEN
	   c = P1C: p$ = P1S$
	ELSE
	   c = P2C: p$ = P2S$
	END IF
	PrintStr MoveList(i, 1) * 10 + 1, MoveList(i, 2) * 10 + 1, c, p$
     END IF
 NEXT i
 PCOPY 0, 1
END SUB

SUB WaitKeyUp
 WHILE MouseK: GetMouse: WEND
END SUB

SUB WaitMove
 GetMouse
 x% = MouseX: y% = MouseY: k% = MouseK
 DO
   GetMouse
   IF MusicDone THEN
      CheckPlaySong
   END IF
 LOOP UNTIL x% <> MouseX OR y% <> MouseY OR k% <> MouseK
END SUB

SUB WhoIsWinner
 Mouse 0
 PCOPY 1, 0

 DIM Score1, Score2 AS DOUBLE

 FOR x = 1 TO 30
     FOR y = 1 TO 18
	 SELECT CASE Board(x, y)
		CASE 1: Score1 = Score1 + CalculatePoints(x, y, 0, 0)
		CASE 2: Score2 = Score2 + CalculatePoints(x, y, 0, 0)
	 END SELECT
     NEXT y
 NEXT x

 IF Moves = 0 THEN
    percent = .5
 ELSE
    percent = Score1 / (Score1 + Score2)
 END IF

 Button 89, 69, 230, 130, 0
 PrintStr 100, 80, 7, "Chance of winning:"
 PrintStr 100, 95, P1C, P1S$
 PrintStr 100, 110, P2C, P2S$
 LINE (110, 95)-(110 + percent * 110, 100), P1C, BF
 LINE (110, 110)-(110 + (1 - percent) * 110, 115), P2C, BF

 Mouse 1                        'show cursor of mouse
 DO: WaitMove
     x = MouseX * 4 - 4
     y = MouseY * 8 - 8
     IF MouseK = 1 THEN EXIT SUB
 LOOP
END SUB

FUNCTION WinCheck (p AS INTEGER)
 DIM cnt(1 TO 4)
 WinCheck = 0
 FOR x = 1 TO 30
     FOR y = 1 TO 18
	 IF Board(x, y) = p THEN
	    FOR i = -2 TO 2
		IF Board(x + i, y + i) = p THEN cnt(1) = cnt(1) + 1
		IF Board(x, y + i) = p THEN cnt(2) = cnt(2) + 1
		IF Board(x + i, y) = p THEN cnt(3) = cnt(3) + 1
		IF Board(x + i, y - i) = p THEN cnt(4) = cnt(4) + 1
	    NEXT i
	    FOR i = 1 TO 4
		IF cnt(i) = 5 THEN
		   WinCheck = 1
		   FOR j = -2 TO 2
		       SELECT CASE i
			 CASE 1: A = (x + j): B = (y + j)
			 CASE 2: A = x: B = (y + j)
			 CASE 3: A = (x + j): B = y
			 CASE 4: A = (x + j): B = (y - j)
		       END SELECT
		       LINE (A * 10 - 1, B * 10 - 1)-(A * 10 + 9, B * 10 + 9), 15, B, &HAAAA
		       LINE (A * 10 - 1, B * 10 - 1)-(A * 10 + 9, B * 10 + 9), 7, B, NOT &HAAAA
		       Delay .5
		   NEXT j
		END IF
		cnt(i) = 0
	    NEXT i
	 END IF
     NEXT y
 NEXT x
END FUNCTION

