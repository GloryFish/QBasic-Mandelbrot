' The Mandelbrot Window for QBasic by Jason Roberts
' 1996 Diamond Soft
' Ver 4.0

' Declare Subs
DECLARE SUB CheckMenuInput ()
DECLARE SUB ClearLine (l%)
DECLARE SUB ClearScreen ()
DECLARE SUB CursorLeft ()
DECLARE SUB CursorRight ()
DECLARE SUB EditSet ()
DECLARE SUB EndProgram ()
DECLARE SUB EraseScanLine ()
DECLARE SUB InitializePalette ()
DECLARE SUB InitializeScreen ()
DECLARE SUB InitializeTimer ()
DECLARE SUB LoadFile ()
DECLARE SUB LoadSet ()
DECLARE SUB MainMenu ()
DECLARE SUB PaintColorGauge ()
DECLARE SUB PaintColorGaugeBlack ()
DECLARE SUB PaintGradient ()
DECLARE SUB PaintIndicatorLights ()
DECLARE SUB PaintLitemodeIndicator ()
DECLARE SUB PaintMainGraphic ()
DECLARE SUB PaintMainMenu ()
DECLARE SUB PaintMandelbrotSet ()
DECLARE SUB PaintScanDot ()
DECLARE SUB PaintSpeedGauge ()
DECLARE SUB Pause ()
DECLARE SUB PrintPercentage ()
DECLARE SUB PrintTimer ()
DECLARE SUB SetColorValues ()
DECLARE FUNCTION GetColorInterval! ()
DECLARE FUNCTION GetColorValue! ()
DECLARE FUNCTION GetIntervalSpeed! ()

'                               *** Types***

TYPE OSetType
     setnum AS INTEGER
     focusx AS CURRENCY
     focusy AS CURRENCY
     zoom AS CURRENCY
     description AS STRING * 20
END TYPE

DIM SHARED OSet AS OSetType ' This stores the set of variables used to create
                            ' a fractal in The Mandelbrot Window.


'                             *** Variables ***

' Arrays
     DIM SHARED redvalue(0 TO 15) AS INTEGER   ' Red value for palette.
     DIM SHARED greenvalue(0 TO 15) AS INTEGER ' Green value for palette.
     DIM SHARED bluevalue(0 TO 15) AS INTEGER  ' Blue value for palette.

' Single Value
     DIM SHARED intervalcounter AS INTEGER ' Holds number of iterations of
                                           ' of the Mandelbrot routine.
     DIM SHARED palettecounter AS INTEGER ' Holds current attribute number
                                          ' during palette initialization.

     DIM SHARED screenwidth AS INTEGER ' The width of the drawing window.
                                       ' Used for multiple screen modes.
     DIM SHARED screenheight AS INTEGER ' The height of the drawing window.
                                        ' Used for multiple screen modes.

     DIM SHARED mandoffsetx AS INTEGER ' The horizontal offset of the
                                       ' Mandelbrot window.
     DIM SHARED mandoffsety AS INTEGER ' The vertical offset of the
                                       ' Mandelbrot window.
     DIM SHARED MandX AS INTEGER ' Holds current column number.
     DIM SHARED MandY AS INTEGER ' Holds current row number.
     DIM SHARED gradientx AS INTEGER ' The X coordinates of the upper left
                                     ' corner of the gradient bar.
     DIM SHARED gradienty AS INTEGER ' The Y coordinates of the upper left
                                     ' corner of the gradient bar.
     DIM SHARED scantype AS INTEGER ' This is used to determine the type of
                                    ' point tracing effect used.
     DIM SHARED count AS INTEGER ' Stores the max number of iterations to
                                 ' perform on a single set of C values.
     DIM SHARED menuchoice AS INTEGER ' Stores the current menu cursor
                                      ' position.
     DIM SHARED ch1x AS INTEGER ' Stores X coordinates of the menu choices.
     DIM SHARED ch2x AS INTEGER
     DIM SHARED ch3x AS INTEGER
     DIM SHARED ch4x AS INTEGER
     DIM SHARED percentage AS LONG ' Stores percentage of the completed set.
     DIM SHARED starttime AS LONG ' Stores program start time.
     DIM SHARED elapsedtime AS LONG ' Stores the total amount of time elapsed.
     DIM SHARED focusx AS SINGLE ' Stores the x value of the window focus.
     DIM SHARED focusy AS SINGLE ' Stores the y value of the window focus.
     DIM SHARED zoom AS SINGLE ' Length\width of the Mandelbrot Window in the
                             ' complex plane.
     DIM SHARED menukey AS STRING ' Stores the key pressed while making a menu
                                 ' choice.
     DIM SHARED startpainting AS INTEGER ' A cheater variable for exiting the
                                         ' menu sub and starting the
                                         ' Mandelbrot routine. Don't ask.
     DIM SHARED setfile AS STRING ' Stores the path of the current set file.
     DIM SHARED litemode1 AS INTEGER ' Stores litemode switch.
     DIM SHARED litemode2 AS INTEGER ' Stores litemode switch.

'                        *** Preassigned Variables ***
setfile = "mandel.set" ' This is the default setfile.
litemode1 = 1
litemode2 = 0

'                            *** Main Program ***

MainMenu ' This is the program's main menu.

ClearScreen ' Clears the screen.

InitializeScreen ' Set screen to desired screen mode (12) and set up
                 ' certain screen features.

SetColorValues ' Stores the RGB values for each color in the palette.

InitializePalette ' Sets palette attributes to the previously set RGB values.
            
PaintMandelbrotSet ' Carries out the Mandelbrot formula and other graphical
                   ' subs.

Pause ' Waits until user presses a key.

EndProgram ' Ends program.

SUB CheckMenuInput

SELECT CASE menukey

CASE CHR$(13)

IF menuchoice = 1 THEN startpainting = 1: EXIT SUB
IF menuchoice = 2 THEN LoadSet: MainMenu
IF menuchoice = 3 THEN EditSet: MainMenu
IF menuchoice = 4 THEN EndProgram

CASE CHR$(0) + CHR$(75)
CursorLeft

CASE CHR$(0) + CHR$(77)
CursorRight


END SELECT

END SUB ' CheckMenuInput

SUB ClearLine (l%)

LOCATE l%
PRINT "                                                                               "

END SUB ' ClearLine

SUB ClearScreen

CLS ' Clear the screen.

END SUB ' ClearScreen

SUB ComputeMandelbrotSet ' Calculate the iteration values for a Mandelbrot
                         ' array. Rather than printing the results it saves
                         ' the coordinates and intervals in a sequential
                         ' access file used by the "spinner", "squisher",
                         ' and "three dimensional linear extrusion procedure."

' To be added: A routine that stores the x screen coordinate, y screen
' coordinate, and the corrosponding interval, every time the Mandelbrot
' routine loops.

count = 50 ' The number of times to iterate the Mandelbrot equations.

InitializeTimer

FOR MandX = 1 TO screenwidth ' Loop the X value of the Mandelbrot set.
     FOR MandY = 1 TO screenheight ' Loop the Y value of the Mandelbrot set.
          C1 = OSet.focusx + OSet.zoom * MandX / screenwidth ' Convert the
                                            ' current MandX value to screen
                                            ' coordinates and store that in
                                            ' C1.
          C2 = OSet.focusy - OSet.zoom * MandY / screenheight ' Convert the
                                            ' current MandY value to screen
                                            ' coordinates and store that in
                                            ' C2.
          x = C1 ' Shift the value of C1 to X so that it may be maintained.
          y = C2 ' Shift the value of C2 to Y so that it may be maintained.
               FOR n = 1 TO count ' Begin the iterations of the Mandelbrot
                                   ' equation.
               x1 = x * x - y * y + C1 ' Calculate the X value and store it
                                       ' in x1.
               y1 = 2 * x * y + C2     ' Calculate the Y value and store it
                                       ' in y1.
               r = x1 * x1 + y1 * y1   ' Calculate the distance from the
                                       ' origin.
               IF r > 4 THEN GOTO skipman2 ' If (c1, c2) tends toward infinity
                                           ' exit the loop.
               x = x1 ' Take the new value for x1 and shift it to x.
               y = y1 ' Take the new value for y1 and shift it to y.
               intervalcounter = n ' Set intervalcounter to the current
                                   ' interval number for use with
                                   ' GetColorInterval
               NEXT n ' Reiterate the Mandelbrot equation.

skipman2:
     q$ = UCASE$(INKEY$)
     IF q$ = CHR$(27) THEN EndProgram ' End program when the user presses
                                      ' Escape.
     NEXT MandY ' Loop to the next MandY value.
NEXT MandX ' Loop to the next MandX value.

END SUB ' ComputeMandelbrotSet

SUB CursorLeft

SELECT CASE menuchoice

CASE 1

menuchoice = 4

CASE 2

menuchoice = 1

CASE 3

menuchoice = 2

CASE 4

menuchoice = 3

END SELECT

END SUB ' CursorLeft

SUB CursorRight

SELECT CASE menuchoice

CASE 1


menuchoice = 2

CASE 2

menuchoice = 3

CASE 3

menuchoice = 4

CASE 4

menuchoice = 1

END SELECT

END SUB ' CursorRight

SUB EditSet

ClearScreen ' Clear the screen.

LOCATE 2, 25 ' Place the cursor.

LOCATE 2, 40 - (1 / 2 * LEN(setfile + "Choose a saved set to load. - "))
                                                  ' Place the cursor.
PRINT "Choose a saved set to load. - "; setfile

LINE (150, 40)-(460, 40)

reread:

OPEN setfile FOR RANDOM AS #1 LEN = LEN(OSet) ' Open the file containing
                                              ' the nine saved sets.

FOR recnum = 1 TO 9 ' There are nine records.
GET #1, recnum, OSet ' Get a record.
LOCATE 3 + recnum, 3 ' Place the cursor.
'Display the set's values.
PRINT OSet.setnum; "."; OSet.description; OSet.focusx, OSet.focusy, OSet.zoom
NEXT recnum ' Go to the next record in the file.

cursy = 1 ' Set starting cursor point.

repaint2:

LOCATE 3 + cursy, 2 ' Place the cursor.
COLOR 4
PRINT "" ' Print a red triangular cursor.
COLOR 15

DO
a$ = INKEY$ ' Get input
LOOP UNTIL a$ <> ""

LOCATE cursy + 3, 2: PRINT " " ' Clear cursor.

IF a$ = CHR$(27) THEN CLOSE #1: ClearScreen: EXIT SUB

IF a$ = CHR$(0) + CHR$(72) THEN ' Cursor up.
cursy = cursy - 1
IF cursy < 1 THEN cursy = 9:
GOTO repaint2
END IF

IF a$ = CHR$(0) + CHR$(80) THEN ' Cursor down.
cursy = cursy + 1
IF cursy > 9 THEN cursy = 1
GOTO repaint2
END IF

IF a$ = CHR$(13) THEN

a% = cursy
LOCATE 18, 2
INPUT "Focus-X:", B@: ClearLine (18)
LOCATE 18, 2
INPUT "Focus-Y:", c@: ClearLine (18)
LOCATE 18, 2
INPUT "Zoom:", d@: ClearLine (18)
LOCATE 18, 2
INPUT "Description (30 chars. or less):", e$: ClearLine (18)

OSet.setnum = a%
OSet.focusx = B@
OSet.focusy = c@
OSet.zoom = d@
OSet.description = e$

PUT #1, OSet.setnum, OSet

END IF

CLOSE #1 ' Close mandel.set

GOTO reread


END SUB ' EditSet

SUB EndProgram

ClearScreen

PRINT "The Mandelbrot Window"
PRINT "by Jason Roberts"
PRINT "1999, 2000 Diamond Soft"

END ' Terminates program.

END SUB ' EndProgram

SUB EraseScanLine

LINE (MandX + mandoffsetx + 1, MandY + mandoffsety - 1)-(screenwidth + mandoffsetx, MandY + mandoffsety - 1), 15

IF MandY + mandoffsety + 2 = screenheight + mandoffsety THEN LINE (MandX + mandoffsetx + 1, MandY + mandoffsety + 2)-(screenwidth + mandoffsetx, MandY + mandoffsety + 1), 3

END SUB ' EraseScanLine

FUNCTION GetColorInterval

SELECT CASE intervalcounter ' Takes current iteration number and determines
                            ' where in the color gradient it falls.

CASE 0 TO 5
     GetColorInterval = 1
CASE 6 TO 10
     GetColorInterval = 2
CASE 11 TO 15
     GetColorInterval = 3
CASE 16 TO 20
     GetColorInterval = 4
CASE 21 TO 25
     GetColorInterval = 5
CASE 26 TO 30
     GetColorInterval = 6
CASE 31 TO 35
     GetColorInterval = 7
CASE 36 TO 40
     GetColorInterval = 8
CASE 41 TO 45
     GetColorInterval = 9
CASE 46 TO 50
     GetColorInterval = 10

END SELECT

END FUNCTION ' GetColorInterval

FUNCTION GetColorValue

pcount% = palettecounter ' Set the temporary variable (pcount%) to
                         ' the current attribute number (palettecounter).

' The following is the equation that calculates the color value to be used
' with the PALETTE statement. It uses the previously assigned RGB values.

GetColorValue = 65536 * bluevalue(pcount%) + 256 * greenvalue(pcount%) + redvalue(pcount%)

END FUNCTION ' GetColorValue

FUNCTION GetIntervalSpeed

GetIntervalSpeed = (200 * intervalcounter) / count

END FUNCTION ' GetIntervalSpeed

SUB InitializePalette


FOR palettecounter = 1 TO 10          ' Loop through each attribute...

PALETTE palettecounter, GetColorValue ' Set the color based on the RGB values.

NEXT palettecounter                   ' Go to next attribute.

END SUB ' InitializePalette

SUB InitializeScreen

scantype = 2 ' This sets the type of point tracing effect used. I couldn't
             ' find a better place to put it.

mode% = 12 ' The desired screen mode. Currently supports Mode 12 only.
           ' I have no plans to add a new mode.
SCREEN mode% ' Set screen to mode%.

SELECT CASE mode% ' Set certain variables according to current screen mode.

CASE 12 ' If it's screen mode 12 then...

screenwidth = 440 ' The width of the Mandelbrot window.
screenheight = 440 ' The height of the Mandelbrot window.

mandoffsetx = 100 ' The horizontal offset from the screen origin.
mandoffsety = 20 ' The vertical offset from the screen origin.

LINE (mandoffsetx - 1, mandoffsety - 1)-(screenwidth + 2 + mandoffsetx, screenheight + 1 + mandoffsety + 1), , BF
                              ' Create a box around the Mandelbrot window.
LINE (screenwidth + mandoffsetx + 5, screenheight + mandoffsety - 130)-(screenwidth + mandoffsetx + 40, screenheight + mandoffsety + 1), , B
                              ' Create a box to hold the current color gauge.

LINE (622, 17)-(643, 441), , B
LINE (575, 150)-(622, 150)
LINE (575, 150)-(575, 220)
LINE (550, 220)-(615, 220)
LOCATE 15, 70
PRINT "Gradient"
'
LINE (590, 305)-(590, 400)
LINE (580, 400)-(590, 400)
LINE (550, 305)-(615, 305)
LOCATE 18, 70
PRINT "Current"
LOCATE 19, 71
PRINT "Color"

LINE (550, 45)-(615, 65), , B ' Timer/Percentage box.
LOCATE 8, 71
PRINT "Timer"
LINE (557, 110)-(600, 110)
LINE (580, 110)-(580, 65)

LINE (60, 30)-(85, 232), , B
LINE (0, 80)-(40, 80)
LINE (20, 81)-(20, 250)
LINE (20, 250)-(72, 250)
LINE (72, 250)-(72, 232)

LOCATE 5, 1
PRINT "Speed"
LOCATE 3, 6
PRINT "10"
LOCATE 9, 7
PRINT "5"
LOCATE 15, 7
PRINT "1"

'
LOCATE 18
PRINT "Litemode"
LOCATE 20, 2
PRINT "On  Off"



LOCATE 1, 7
PRINT "The Mandelbrot Window Ver. 3.0 by Jason Roberts - 1999 Diamond Soft"
'

CASE ELSE ' If mode% isn't a valid screen mode...

ClearScreen

PRINT "Invalid Screen Mode"
PRINT "Current version supports mode 12 only."

EndProgram

END SELECT


PaintGradient ' Paint the gradient bar based on the location of the
              ' Mandelbrot window.

END SUB ' InitializeScreen

SUB InitializeTimer

starttime = TIMER

END SUB ' InitializeTimer

SUB LoadSet

loadclear:
loadsetfile:

ClearScreen ' Clear the screen.

LOCATE 2, 40 - (1 / 2 * LEN(setfile + "Choose a saved set to load. - "))
                                                  ' Place the cursor.
PRINT "Choose a saved set to load. - "; setfile

LINE (150, 40)-(460, 40)

OPEN setfile FOR RANDOM AS #1 LEN = LEN(OSet) ' Open the file containing
                                                   ' the nine saved sets.
displayset:

FOR recnum = 1 TO 9 ' There are nine records.
GET #1, recnum, OSet ' Get a record.
LOCATE 3 + recnum, 3 ' Place the cursor.
'Display the set's values.
PRINT OSet.setnum; "."; OSet.description; OSet.focusx, OSet.focusy, OSet.zoom
NEXT recnum ' Go to the next record in the file.
LOCATE 3 + recnum + 1, 4
'PRINT "Load new set file."

cursy = 1 ' Set starting cursor point.

repaint:

LOCATE 3 + cursy, 2 ' Place the cursor.
COLOR 4
PRINT "" ' Print a red triangular cursor.
COLOR 15

DO
a$ = INKEY$ ' Get input
LOOP UNTIL a$ <> ""

LOCATE cursy + 3, 2: PRINT " " ' Clear cursor.

IF a$ = CHR$(0) + CHR$(72) THEN ' Cursor up.
cursy = cursy - 1
IF cursy < 1 THEN cursy = 9:
GOTO repaint
END IF

IF a$ = CHR$(0) + CHR$(80) THEN ' Cursor down.
cursy = cursy + 1
IF cursy = 10 THEN cursy = 1
GOTO repaint
END IF

IF a$ = CHR$(13) THEN ' If Enter key is pressed...

IF cursy <> 11 THEN   ' See if the cursor is at "Load new setfile."...
GET #1, cursy, OSet   ' If not, then load the current record for use.
ELSE                  ' If "Load new setfile." has been chosen...

LOCATE 3 + recnum + 2, 4 ' Place cursor just under "Load new setfile."
LINE INPUT "Type new path and file name, leave blank to cancel.", tempsetfile$
               ' Get new path and filename.
IF tempsetfile$ = "" THEN ' If tempsetfile$ is blank...
                          
GOTO loadsetfile ' Then reload set without changing path and filename.

ELSE ' If tempsetfile$ isn't blank...

IF tempsetfile$ = setfile THEN GOTO displayset ' If the same path and filename
                                            ' are given then return to the
                                            ' top.

errorchecksetfile$ = setfile
setfile = tempsetfile$: CLOSE #1: GOTO loadsetfile ' Switch the path and
                                                   ' filename and reload.

END IF
END IF
END IF

CLOSE #1 ' Close current set.

ClearScreen

END SUB ' LoadSet

SUB MainMenu

PaintMainMenu ' Paint the "Mandelbrot Window" title graphic.

menuchoice = 1 ' Set the menu cursor to the first choice.

menuLoop:

PaintIndicatorLights ' Paint active button green and inactive buttons red.

DO ' Begin menu loop.

menukey = UCASE$(INKEY$) ' Get input.

LOOP UNTIL menukey <> "" ' End loop when a key is pressed.

CheckMenuInput

IF startpainting = 1 THEN EXIT SUB

GOTO menuLoop

EndProgram

END SUB ' MainMenu

SUB PaintColorGauge

' Fill in the color gauge with the color that is currently being placed.

PAINT (screenwidth + mandoffsetx + 6, screenheight + mandoffsety), GetColorInterval, 15

END SUB ' PaintColorGauge

SUB PaintColorGaugeBlack

' Fill in the color gauge with black, instead of colors in the gradient.

PAINT (screenwidth + mandoffsetx + 6, screenheight + mandoffsety), 0, 15

END SUB ' PaintColorGaugeBlack

SUB PaintGradient

gradientx = 625 ' The X coordinates of the upper left corner of the gradient
                ' bar.
col% = 1 ' Set temporary variable (col%) to 1 for upcoming loop.

FOR gradienty = mandoffsety TO mandoffsety + 400 STEP 42
                                   ' Loop through the Y coordinates of the
                                   ' gradient starting at (mandoffsety).
LINE (gradientx, gradienty)-(gradientx + 30, gradienty + 40), col%, BF
                                   ' Paint current gradient box with col%.
col% = col% + 1 ' Increment col%.
NEXT gradienty ' Loop to next gradienty value.

END SUB ' PaintGradient

SUB PaintIndicatorLights

SELECT CASE menuchoice

CASE 1

PAINT (ch1x, 280), 2, 15
PAINT (ch2x, 280), 4, 15
PAINT (ch3x, 280), 4, 15
PAINT (ch4x, 280), 4, 15

CASE 2

PAINT (ch1x, 280), 4, 15
PAINT (ch2x, 280), 2, 15
PAINT (ch3x, 280), 4, 15
PAINT (ch4x, 280), 4, 15

CASE 3

PAINT (ch1x, 280), 4, 15
PAINT (ch2x, 280), 4, 15
PAINT (ch3x, 280), 2, 15
PAINT (ch4x, 280), 4, 15

CASE 4

PAINT (ch1x, 280), 4, 15
PAINT (ch2x, 280), 4, 15
PAINT (ch3x, 280), 4, 15
PAINT (ch4x, 280), 2, 15

END SELECT


END SUB ' PaintIndicatorLights

SUB PaintLitemodeIndicator

IF litemode1 = 0 THEN LINE (6, 304)-(24, 317), 14, B: LINE (38, 304)-(63, 317), 0, B

IF litemode1 = 1 THEN LINE (38, 304)-(63, 317), 14, B: LINE (6, 304)-(24, 317), 0, B

END SUB ' PaintLitemodeIndicator

SUB PaintMainGraphic

'Picture Plus
'by Chris Howie
'1998,1999 Crazy Computers

SHELL "lppdgraf.exe" ' This is the program that loads the Picture Plus document.

END SUB ' PaintMainGraphic

SUB PaintMainMenu

SCREEN 12

PaintMainGraphic

LINE (9, 9)-(629, 169), , B

LOCATE 15
PRINT "  Create Image              Load Set              Edit Set                Quit"

LINE (10, 220)-(630, 240), , B ' Line around menu choices.

ch1x% = 67
ch2x% = 255
ch3x% = 435
ch4x% = 610

CIRCLE (ch1x%, 280), 25 ' Choice indicator "lights".
CIRCLE (ch1x%, 280), 24, 8
CIRCLE (ch1x%, 280), 23
CIRCLE (ch1x%, 280), 18, , 1.5, 3.7

CIRCLE (ch2x%, 280), 25
CIRCLE (ch2x%, 280), 24, 8
CIRCLE (ch2x%, 280), 23
CIRCLE (ch2x%, 280), 18, , 1.5, 3.7

CIRCLE (ch3x%, 280), 25
CIRCLE (ch3x%, 280), 24, 8
CIRCLE (ch3x%, 280), 23
CIRCLE (ch3x%, 280), 18, , 1.5, 3.7

CIRCLE (ch4x%, 280), 25
CIRCLE (ch4x%, 280), 24, 8
CIRCLE (ch4x%, 280), 23
CIRCLE (ch4x%, 280), 18, , 1.5, 3.7

LINE (ch1x%, 320)-(ch4x%, 320) ' Line top.
CIRCLE (ch1x%, 323), 2, , 2, 4 ' Line left.
LINE (ch1x%, 326)-(ch4x%, 326) ' Line bottom.
CIRCLE (ch4x%, 323), 2, , 6, 1  ' Line left.

END SUB ' PaintMainMenu

SUB PaintMandelbrotSet

count = 50 ' The number of times to iterate the Mandelbrot equations.

InitializeTimer
PaintLitemodeIndicator

FOR MandX = 1 TO screenwidth ' Loop the X value of the Mandelbrot set.
     FOR MandY = 1 TO screenheight ' Loop the Y value of the Mandelbrot set.
          IF litemode1 = 1 THEN PaintSpeedGauge ' Paint a gauge with a scaled speed representation.
          IF litemode1 = 1 THEN PrintTimer ' Prints the number of seconds spent on the fractal.
          C1 = OSet.focusx + OSet.zoom * MandX / screenwidth ' Convert the
                                            ' current MandX value to screen
                                            ' coordinates and store that in
                                            ' C1.
          C2 = OSet.focusy - OSet.zoom * MandY / screenheight ' Convert the
                                            ' current MandY value to screen
                                            ' coordinates and store that in
                                            ' C2.
          x = C1 ' Shift the value of C1 to X so that it may be maintained.
          y = C2 ' Shift the value of C2 to Y so that it may be maintained.
               FOR n = 1 TO count ' Begin the iterations of the Mandelbrot
                                   ' equation.
               x1 = x * x - y * y + C1 ' Calculate the X value and store it
                                       ' in x1.
               y1 = 2 * x * y + C2     ' Calculate the Y value and store it
                                       ' in y1.
               r = x1 * x1 + y1 * y1   ' Calculate the distance from the
                                       ' origin.
               IF r > 4 THEN GOTO skipman ' If (c1, c2) tends toward infinity
                                          ' exit the loop.
               x = x1 ' Take the new value for x1 and shift it to x.
               y = y1 ' Take the new value for y1 and shift it to y.
               intervalcounter = n ' Set intervalcounter to the current
                                   ' interval number for use with
                                   ' GetColorInterval
               NEXT n ' Reiterate the Mandelbrot equation.

                         PSET (MandX + mandoffsetx, MandY + mandoffsety), 0: IF litemode1 = 1 THEN PaintColorGaugeBlack
                                   ' If the point remains within the 4 x 4
                                   ' boundaries then color the point black
                                   ' and color the gauge black.
skipman:

     IF r > 4 THEN PSET (MandX + mandoffsetx, MandY + mandoffsety), GetColorInterval: IF litemode1 = 1 THEN PaintColorGauge
                                   ' If the point tends to infinity color it
                                   ' based on the number of iterations of the
                                   ' Mandelbrot equation.
     q$ = UCASE$(INKEY$)
     IF q$ = CHR$(27) THEN EndProgram ' End program when the user presses
                                        ' Escape.
     IF q$ = " " THEN SWAP litemode1, litemode2: PaintLitemodeIndicator
                         ' Turn litemode on or off, then indicate.
     
     IF litemode1 = 1 THEN IF scantype = 1 THEN EraseScanLine
     IF litemode1 = 1 THEN IF scantype = 2 THEN PaintScanDot

     NEXT MandY ' Loop to the next MandY value.
NEXT MandX ' Loop to the next MandX value.

END SUB ' PaintMandelbrotSet

SUB PaintScanDot

' Paints a dot at the current point. This is much faster than the line.

PSET (MandX + mandoffsetx, MandY + mandoffsety + 1), 15

END SUB ' PaintScanDot

SUB PaintSpeedGauge

intspeed% = GetIntervalSpeed ' Precalculating the speed saves time.

LINE (61, 231)-(84, 31 + intspeed%), 1, BF
LINE (61, 31)-(84, 31 + intspeed%), 0, BF

END SUB ' PaintSpeedGauge

SUB Pause

DO: LOOP UNTIL INKEY$ <> "" ' Wait until user presses a key.

END SUB ' Pause

SUB PrintPercentage

percentage = (100 * MandX) / screenwidth ' Calculate the percentage of the
                                         ' completed image.
LOCATE 4, 70 ' Set text cursor position.
PRINT percentage; "%" ' Print the percentage with a "%" following it.

END SUB ' PrintPercentage

SUB PrintTimer

elapsedtime = TIMER - starttime

secondhalf = elapsedtime MOD 60
firsthalf = (elapsedtime - secondhalf) / 60

LOCATE 4, 72
PRINT " :  "

LOCATE 4, 72
PRINT LTRIM$(STR$(firsthalf) + ":" + LTRIM$(STR$(secondhalf)))

END SUB ' PrintTimer

SUB SelectChoice


END SUB ' SelectChoice

SUB SetColorValues

' The following are the RGB values used by the function GetColorValue to
' compute the color values for the PALLETTE statement.

' Attribute 1
redvalue(1) = 40
greenvalue(1) = 10
bluevalue(1) = 0

' Attribute 2
redvalue(2) = 50
greenvalue(2) = 15
bluevalue(2) = 0

' Attribute 3
redvalue(3) = 50
greenvalue(3) = 20
bluevalue(3) = 0

' Attribute 4
redvalue(4) = 50
greenvalue(4) = 25
bluevalue(4) = 0

' Attribute 5
redvalue(5) = 60
greenvalue(5) = 35
bluevalue(5) = 0

' Attribute 6
redvalue(6) = 60
greenvalue(6) = 40
bluevalue(6) = 0

' Attribute 7
redvalue(7) = 63
greenvalue(7) = 50
bluevalue(7) = 0

' Attribute 8
redvalue(8) = 30
greenvalue(8) = 60
bluevalue(8) = 0

' Attribute 9
redvalue(9) = 0
greenvalue(9) = 40
bluevalue(9) = 10

' Attribute 10
redvalue(10) = 0
greenvalue(10) = 30
bluevalue(10) = 15

END SUB ' SetColorValues

