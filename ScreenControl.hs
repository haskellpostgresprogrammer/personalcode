module ScreenControl where

import Utils

xdo s = spaces ["xdotool",s]
mousemove x y = 
  xdo $ spaces ["mousemove",show x,show y]
click = xdo "click 1"
key x = xdo $ spaces ["key",x]
sleep x = spaces ["sleep",show x]
commands l = intercalate ";" l

xmonad1 = key "super+1"
xmonad2 = key "super+2"
pageDown = key "Next"
pageUp = key "Prior"
conkeror = xmonad2
web = conkeror
emacs = xmonad1
conkerorFind s = delayed 2 [key "f",keyString s]
conk s = conkerorFind s
top = key "Home"
bottom = key "End"
enterKey = key "Return"
enterk = enterKey
enter s = delayed 0.2 [keyString s,enterKey]
tab = key "Tab"

textZoom = key "plus"
pageZoomUp = key "ctrl+shift+plus"
zoom n = delayed 0.3 $ replicate n pageZoomUp

conkerorOpen s = spaces ["conkeror",squoted s]
conkopen s = conkerorOpen s

delayed n l = commands $ intersperse (sleep n) l

hackernews = 
--  delayed 3 [conkerorFind "hacker",enterKey]
  delayed 3 
  [web,conkopen "news.ycombinator.com",zoom 3]
  
-- use grided on screenshot to find coords for clck
-- install lightweight browser that can open kaggle
--    and googleplus
  
width = 800
height =  600
xPercent n = (width / 100) * n
yPercent n = height - ((height / 100) * n)
xp n = xPercent n
yp n = yPercent n
leftScrollButton = 
  delayed 0.3 [mousemove (xp 1) (yp 5)]
rightScrollButton = 
  delayed 0.3 [web,mousemove (xp 97) (yp 5)]
clickn n = delayed 0.1 $ replicate n click

main = run $ delayed 2
       [rightScrollButton,clickn 20,
        leftScrollButton,clickn 20,
        emacs]
  
mx = key "alt+x"
conkmx = delayed 1 [web,mx]

currentPage = key "0"
beginningOfLine = key "ctrl+a"
killLine = key "ctrl+k"
killWholeLine = delayed 0.5 [beginningOfLine,killLine]
conksave fname = 
  delayed 2
  [web,key "s",currentPage,killWholeLine,
   enter $ homedir fname]

google s = 
  delayed 2 
  [conkerorOpen "google.com",enter s,zoom 2]

homedir s = "/home/umar/" ++ s

screenshot pic = 
  delayed 1 [web,scrot pic,emacs,showpic pic]
scrot fname = spaces ["scrot",homedir fname]
showpic fname = spaces ["xli",homedir fname]

keyString s = delayed 0.1 $ map (key . keycode) s
keycode c = 
  case c of
    ' ' -> "space"
    '/' -> "slash"
    '\\' -> "backslash"
    '.' -> "period"
    _ -> show c
    
run s = do
  print s
  shell s

--  [web,google "reddit oculus rift",conk "reddit",tab,hit]

-- scrot -cd 5 desktop.jpg (png give whitish color)
-- xli desktop.jpg

-- xdotool
-- xdotool click 1

-- Usage: xdotool <cmd> <args>
-- Available commands:
--   getactivewindow
--   getwindowfocus
--   getwindowpid
--   search
--   help
--   version
--   click
--   getmouselocation
--   key
--   keydown
--   keyup
--   mousedown
--   mousemove
--   mousemove_relative
--   mouseup
--   type
--   windowactivate
--   windowfocus
--   windowmap
--   windowmove
--   windowraise
--   windowsize
--   windowunmap
--   set_window
--   set_num_desktops
--   get_num_desktops
--   set_desktop
--   get_desktop
--   set_desktop_for_window
--   get_desktop_for_window

-- http://www.cl.cam.ac.uk/~mgk25/ucs/X11.keysyms

-- #xFF08	BACKSPACE, BACK SPACE, BACK CHAR	Keyboard
-- #xFF09	TAB	Keyboard
-- #xFF0A	LINEFEED, LF	Keyboard
-- #xFF0B	CLEAR	Keyboard
-- #xFF0D	RETURN, ENTER	Keyboard
-- #xFF13	PAUSE, HOLD	Keyboard
-- #xFF14	SCROLL LOCK	Keyboard
-- #xFF15	SYS REQ, SYSTEM REQUEST	Keyboard
-- #xFF1B	ESCAPE	Keyboard
-- #xFF20	MULTI-KEY CHARACTER PREFACE	Keyboard
-- #xFF50	HOME	Keyboard
-- #xFF51	LEFT, MOVE LEFT, LEFT ARROW	Keyboard
-- #xFF52	UP, MOVE UP, UP ARROW	Keyboard
-- #xFF53	RIGHT, MOVE RIGHT, RIGHT ARROW	Keyboard
-- #xFF54	DOWN, MOVE DOWN, DOWN ARROW	Keyboard
-- #xFF55	PRIOR, PREVIOUS, PAGE UP	Keyboard
-- #xFF56	NEXT, PAGE DOWN	Keyboard
-- #xFF57	END, EOL	Keyboard
-- #xFF58	BEGIN, BOL	Keyboard
-- #xFF60	SELECT, MARK	Keyboard
-- #xFF61	PRINT	Keyboard
-- #xFF62	EXECUTE, RUN, DO	Keyboard
-- #xFF63	INSERT, INSERT HERE	Keyboard
-- #xFF65	UNDO, OOPS	Keyboard
-- #xFF66	REDO, AGAIN	Keyboard
-- #xFF67	MENU	Keyboard
-- #xFF68	FIND, SEARCH	Keyboard
-- #xFF69	CANCEL, STOP, ABORT, EXIT	Keyboard
-- #xFF6A	HELP	Keyboard
-- #xFF6B	BREAK	Keyboard
-- #xFF7E	MODE SWITCH, SCRIPT SWITCH, CHARACTER SET SWITCH	Keyboard
-- #xFF7F	NUM LOCK	Keyboard
-- #xFF80	KEYPAD SPACE	Keyboard
-- #xFF89	KEYPAD TAB	Keyboard
-- #xFF8D	KEYPAD ENTER	Keyboard
-- #xFF91	KEYPAD F1, PF1, A	Keyboard
-- #xFF92	KEYPAD F2, PF2, B	Keyboard
-- #xFF93	KEYPAD F3, PF3, C	Keyboard
-- #xFF94	KEYPAD F4, PF4, D	Keyboard
-- #xFF95	KEYPAD HOME	Keyboard
-- #xFF96	KEYPAD LEFT	Keyboard
-- #xFF97	KEYPAD UP	Keyboard
-- #xFF98	KEYPAD RIGHT	Keyboard
-- #xFF99	KEYPAD DOWN	Keyboard
-- #xFF9A	KEYPAD PRIOR, PAGE UP	Keyboard
-- #xFF9B	KEYPAD NEXT, PAGE DOWN	Keyboard
-- #xFF9C	KEYPAD END	Keyboard
-- #xFF9D	KEYPAD BEGIN	Keyboard
-- #xFF9E	KEYPAD INSERT	Keyboard
-- #xFF9F	KEYPAD DELETE	Keyboard
-- #xFFAA	KEYPAD MULTIPLICATION SIGN, ASTERISK	Keyboard
-- #xFFAB	KEYPAD PLUS SIGN	Keyboard
-- #xFFAC	KEYPAD SEPARATOR, COMMA	Keyboard
-- #xFFAD	KEYPAD MINUS SIGN, HYPHEN	Keyboard
-- #xFFAE	KEYPAD DECIMAL POINT, FULL STOP	Keyboard
-- #xFFAF	KEYPAD DIVISION SIGN, SOLIDUS	Keyboard
-- #xFFB0	KEYPAD DIGIT ZERO	Keyboard
-- #xFFB1	KEYPAD DIGIT ONE	Keyboard
-- #xFFB2	KEYPAD DIGIT TWO	Keyboard
-- #xFFB3	KEYPAD DIGIT THREE	Keyboard
-- #xFFB4	KEYPAD DIGIT FOUR	Keyboard
-- #xFFB5	KEYPAD DIGIT FIVE	Keyboard
-- #xFFB6	KEYPAD DIGIT SIX	Keyboard
-- #xFFB7	KEYPAD DIGIT SEVEN	Keyboard
-- #xFFB8	KEYPAD DIGIT EIGHT	Keyboard
-- #xFFB9	KEYPAD DIGIT NINE	Keyboard
-- #xFFBD	KEYPAD EQUALS SIGN	Keyboard
-- #xFFBE	F1	Keyboard
-- #xFFBF	F2	Keyboard
-- #xFFC0	F3	Keyboard
-- #xFFC1	F4	Keyboard
-- #xFFC2	F5	Keyboard
-- #xFFC3	F6	Keyboard
-- #xFFC4	F7	Keyboard
-- #xFFC5	F8	Keyboard
-- #xFFC6	F9	Keyboard
-- #xFFC7	F10	Keyboard
-- #xFFC8	F11, L1	Keyboard
-- #xFFC9	F12, L2	Keyboard
-- #xFFCA	F13, L3	Keyboard
-- #xFFCB	F14, L4	Keyboard
-- #xFFCC	F15, L5	Keyboard
-- #xFFCD	F16, L6	Keyboard
-- #xFFCE	F17, L7	Keyboard
-- #xFFCF	F18, L8	Keyboard
-- #xFFD0	F19, L9	Keyboard
-- #xFFD1	F20, L10	Keyboard
-- #xFFD2	F21, R1	Keyboard
-- #xFFD3	F22, R2	Keyboard
-- #xFFD4	F23, R3	Keyboard
-- #xFFD5	F24, R4	Keyboard
-- #xFFD6	F25, R5	Keyboard
-- #xFFD7	F26, R6	Keyboard
-- #xFFD8	F27, R7	Keyboard
-- #xFFD9	F28, R8	Keyboard
-- #xFFDA	F29, R9	Keyboard
-- #xFFDB	F30, R10	Keyboard
-- #xFFDC	F31, R11	Keyboard
-- #xFFDD	F32, R12	Keyboard
-- #xFFDE	F33, R13	Keyboard
-- #xFFDF	F34, R14	Keyboard
-- #xFFE0	F35, R15	Keyboard
-- #xFFE1	LEFT SHIFT	Keyboard
-- #xFFE2	RIGHT SHIFT	Keyboard
-- #xFFE3	LEFT CONTROL	Keyboard
-- #xFFE4	RIGHT CONTROL	Keyboard
-- #xFFE5	CAPS LOCK	Keyboard
-- #xFFE6	SHIFT LOCK	Keyboard
-- #xFFE7	LEFT META	Keyboard
-- #xFFE8	RIGHT META	Keyboard
-- #xFFE9	LEFT ALT	Keyboard
-- #xFFEA	RIGHT ALT	Keyboard
-- #xFFEB	LEFT SUPER	Keyboard
-- #xFFEC	RIGHT SUPER	Keyboard
-- #xFFED	LEFT HYPER	Keyboard
-- #xFFEE	RIGHT HYPER	Keyboard
-- #xFFFF	DELETE, RUBOUT	Keyboard
