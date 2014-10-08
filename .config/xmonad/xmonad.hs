-- Andrew Michaud's XMonad config.
-- Built from bits and pieces of other configs by other people.

import XMonad

import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
-- For dealing with Java.
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe, runProcessWithInput)

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

-- For xrandr info processing.
import qualified Data.Char as C
import Data.List

import System.IO
import System.FilePath.Posix
import System.Posix.Unistd -- to get hostname
import System.Environment(getEnv)

-- For extra workspace nonsense
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------------------------------------
------------------------------------------------ MAIN --------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do

    -- Get hostname for system-dependent stuff.
    host <- fmap nodeName getSystemID

    -- Get useful environment variables.
    editor     <- getEnv "EDITOR"
    configHome <- getEnv "XDG_CONFIG_HOME"

    -- Resolution info
    xrandr   <- runProcessWithInput "xrandr" [] ""
    let dims =  screeninfo xrandr

    -- Status bar programs.
    -- Configure statusbars based on how many monitors we have. (up to two, at least)

    -- Left bar - XMonad info.
    let leftw
            | null dims        = 300 --whatever
            | length dims == 1 = head (head dims) `div` 2
            | otherwise        = head $ head dims
    dzenL  <- spawnPipe $ dzenLeft trayWidth (leftw - trayWidth)

    -- Right bar - MPD and other useful status info.
    let rightx = leftw
    let rightw
            | null dims        = 300
            | length dims == 1 = (head (head dims) `div` 2) + 1
            | otherwise        = head (dims !! 1)
    dzenRPID  <- spawnPID $ dzenRight rightx rightw configHome

    -- System tray.
    trayPID <- spawnPID tray

    -- Conky background status info.
    conkyPID  <- spawnPID $ conkyStatus configHome

    -- Config xmonad.
    xmonad $ withUrgencyHook uHook $ defaultConfig

        -- Hooks.
        { manageHook = mHook
        , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
        , logHook    = lHook configHome dzenL

        -- Handles Java
        , startupHook = setWMName "LG3D"

        -- Stuff.
        , modMask    = mMask
        , workspaces = spaces
        , terminal   = term

        -- Mice are for squares.
        , focusFollowsMouse = False

        -- Border jazz
        , focusedBorderColor = focBord
        , normalBorderColor  = normBord
        , borderWidth        = bordWidth

        } `additionalKeys` keybinds configHome host

---------------------------------------------
--------------- VARIABLES AND STUFF
---------------------------------------------

-- Color variables.
-- _ :: String
-- TODO put these in a common file somewhere and refer to them there.
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- Font variables.
-- _ :: String
fontName  = "Dejavu"
font size = "-*-" ++ fontName ++ "-medium-r-normal-*-" ++ show size ++ "-140-75-75-p-74-iso10646-1"
fn size   = " -fn " ++ font size

-- XMonad colors and styling.
-- _ :: String
normFG   = base3
normBG   = base03
currBG   = orange
urgBG    = red
focBord  = orange
normBord = base1
bordWidth :: Dimension
bordWidth = 2

-- Dzen colors.
-- _ :: ???
-- dzenColor takes fg then bg
dzenCurr = dzenColor normFG currBG
dzenNorm = dzenColor normFG normBG
dzenUrg  = dzenColor normFG urgBG

-- dmenu colors
-- _ :: String
normBGDmenu = dmenuWrap normBG
normFGDmenu = dmenuWrap normFG
selFGDmenu  = dmenuWrap normFG
selBGDmenu  = dmenuWrap currBG
dmenuWrap :: String -> String
dmenuWrap str = "'" ++ str ++ "'"

-- dmenu/dzen styles.
-- _ :: String
dmenuStyle = fn 6 ++ " -nb " ++ normBGDmenu ++ " -nf " ++ normFGDmenu ++ " -sf " ++
             selFGDmenu ++ " -sb " ++ selBGDmenu
dzenStyle  = fn 12 ++ " -h '" ++ show statusHeight ++ "' -y '0'"

-- Trayer colors
bgTrayer :: String
bgTrayer = "0x" ++ tail normBG

-- More complicated variables.

-- Keybinds, depending on host for audio keys.
keybinds configHome host =
    [
    -- Screen lock
      ((mMask .|. shiftMask,     xK_l),                     spawn "xscreensaver-command -lock")

    -- Backlight.
    , ((0,                       xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                       xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")

    -- Screenshots.
    , ((0,                       xK_Print),                 spawn $ scrot "")
    , ((controlMask,             xK_Print),                 spawn $ scrot "select")
    , ((shiftMask,               xK_Print),                 spawn $ scrot "delay")

    -- Recompile/restart XMonad. Modified to kill taskbar programs.
    , ((mMask,                   xK_q),                     spawn $ "xmonad --recompile;" ++
                                                                    "killall dzen conky trayer;" ++
                                                                    "xmonad --restart")
    -- Run DMenu.
    , ((mMask,                   xK_p),                     spawn $ "dmenu_run " ++ dmenuStyle)

    -- Quick program spawns.
    , ((mMask,                   xK_a),                     spawn $ namedCmd "alsamixer" "")
    -- TODO- Find a cleaner way to do this.
    , ((mMask,                   xK_o),                     spawn $ namedCmd "ncmpcpp" ("-c " ++
                                                                     configHome ++ "/ncmpcpp/config"))
    , ((mMask,                   xK_b),                     spawn  "x-www-browser")

    -- Various useful scripts.
    , ((mMask .|. shiftMask,     xK_s),                     spawn "~/bin/setWallpaper")
    , ((mMask,                   xK_n),                     spawn "~/bin/toggleOneko")
    -- Clip password with dzen and a nifty script.
    , ((mMask .|. shiftMask,     xK_p),                     spawn $ "~/bin/passmenu " ++ dmenuStyle)
    ] ++

    -- Audio keys.
    audioKeys host ++

    -- Keys for extra workspaces
    [((mMask .|. shiftMask, k), windows $ W.shift i) |
        (i, k) <- zip spaces wsKeys] ++

    [( (mMask,              k), windows $ W.greedyView i) |
        (i, k) <- zip spaces wsKeys]
        where wsKeys  = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

-------------------------------------------------------------------------------------------------------------
-----------------------------STATUS BAR STUFF----------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------

-- Left Dzen - xmonad info and window title.
dzenLeft :: Int -> Int -> String
dzenLeft start width = "dzen2 -x "++ "'" ++ show start ++ "'" ++ " -w '" ++ show width ++
                       "' -ta 'l'" ++ dzenStyle

-- Right Dzen - Runs conky config.
dzenRight :: Int -> Int -> String -> String
dzenRight start width config = conkyCmd ++ " | dzen2 " ++ style
    where conkyCmd = "conky --config=" ++ config ++ "/conky/dzen_config"
          style    = "-x '" ++ show start ++ "' -w '" ++ show width ++ "' -ta 'r'" ++ dzenStyle

-- Background status.
conkyStatus :: String -> String
conkyStatus config = "conky --config=" ++ config ++ "/conky/config"

-- Trayer - system tray.
-- TODO swap in stalonetray
tray :: String
tray = "trayer --edge top --align left --transparent false " ++
       "--widthtype pixel --width " ++ show trayWidth ++ " " ++
       "--expand true --SetDockType true --SetPartialStrut true " ++
       "--heighttype pixel --height " ++ show statusHeight ++ " --padding 2"

-- Bitmaps used to represent current layout.
bmpPath :: String -> String
bmpPath configHome = configHome ++ "/dzen/bitmaps/"

-- Pretty printing.
myDzenPP :: String -> Handle -> PP
myDzenPP configHome h = dzenPP
    { ppOutput  = hPutStrLn h
    , ppCurrent = dzenCurr . pad
    , ppHidden  = dzenNorm . pad . take 1
    , ppUrgent  = dzenUrg  . pad
    , ppSep     = " "
    , ppTitle   = dzenColor normFG normBG . pad
    , ppLayout  = \x -> case x of
                      "Tall"        -> wrapBitmap "rob/tall.xbm"
                      "Mirror Tall" -> wrapBitmap "rob/mtall.xbm"
                      "Full"        -> wrapBitmap "rob/full.xbm"
    }
    where wrapBitmap bitmap = "^p(4)^i(" ++ bmpPath configHome ++ bitmap ++ ")^p(4)"

-- Status bar dimensions.
statusHeight = 16
trayWidth    = 70

-- Terminal commands
term              = "xfce4-terminal --hide-menubar"
termCmd cmd       = term ++ " -x " ++ cmd
namedCmd cmd args = term ++ " --title=" ++ name ++ " -x " ++ cmd ++ " " ++ args
    where name = "__" ++ map C.toUpper cmd

-- Use Windows key as mod key, it's convenient.
mMask = mod4Mask

-- Workspace titles
spaces :: [String]
spaces = ["` term", "1 term", "2 web",  "3 chat", "4 ssh", "5 play", "6 play",  "7 work",
          "8 work", "9 work", "0 etc.", "- perf", "= \956sic"]

-- Audio control keys.
-- Assuming computer has the proper audio keys if it's not my laptop.
audioKeys = \h -> case h of

    -- Laptop doesn't have proper media keys because Lenovo are dumb.
    -- So, do it manually.
    "pascal" -> [ ((shiftMask, xF86XK_AudioMute),        spawn "mpc toggle")
                , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "mpc next")
                , ((shiftMask, xF86XK_AudioLowerVolume), spawn "mpc prev")] ++ common

    _        -> [ ((0, xF86XK_AudioPlay), spawn "mpc toggle")
                , ((0, xF86XK_AudioNext), spawn "mpc next")
                , ((0, xF86XK_AudioPrev), spawn "mpc prev")] ++ common

    where common = [ ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
                   , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
                   , ((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")]

-- Screenshot commands
scrot :: String -> String
scrot = \x -> case x of
                  ""       -> "scrot " ++ format ++ destination
                  "select" -> "scrot " ++ "-s " ++ format ++ destination
                  "delay"  -> "scrot " ++ "--delay 5 " ++ format ++ destination
    where format      = "'%d-%m-%Y-%s_$wx$h.png' "
          destination = "-e 'mv $f ~/pictures/screenshots/' "

--------------------------------------------------------------------------------------------------------------
-------------------------------------UTILITY--FUNCTIONS-------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-- Grab resolution and positioning for each active monitor given xrandr string.
screeninfo :: String -> [[Int]]
screeninfo input = infoints
    where reslines   = filter (\x -> head (tail x ) == "connected" ) $ map words (lines input)
          resstrings = map (map (\x -> if C.isNumber x then x else ' ') . (!!2)) reslines
          info       = map words resstrings
          infoints   = map (map read) info :: [[Int]]

--------------------------------------------------------------------------------------------------------------
----------------------------------CUSTOM HOOKS--------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- Custom urgency hook.
uHook = dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }

-- Custom manage hook.
-- Manage docks, custom layout nonsense.
mHook :: ManageHook
mHook = manageDocks <+> composeAll
    [ className =? "Xfce4-notifyd"  --> doIgnore

    -- Float special command windows.
    , title     =? "__NCMPCPP"      --> doCenterFloat
    , title     =? "__ALSAMIXER"    --> doCenterFloat
    , title     =? "__XMONADHS"     --> doCenterFloat
    , title     =? "__HTOP"         --> doShift "-:perf"

    -- Handle gimp and mpv specially.
    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFullFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin"         --> doShift "3:chat"
    , title     =? "Skype"          --> doShift "3:chat"
    ] <+> manageHook defaultConfig

-- Custom log hook.
-- Forward window information to dzen bar, formatted.
lHook :: String -> Handle -> X ()
lHook config_home h = dynamicLogWithPP (myDzenPP config_home h)

------------------------------------------------------------------------------------------
----------------------------- END CUSTOM HOOKS -------------------------------------------
------------------------------------------------------------------------------------------
