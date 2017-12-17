---------------------------------------------------------------------------------------------------
-- AUTHOR:  Andrew Michaud - https://andrewmichaud.com                                           --
-- FILE:    xmonad.hs                                                                            --
-- PURPOSE: XMonad configuration file.                                                           --
-- UPDATED: 2016-11-26                                                                           --
-- LICENSE: ISC                                                                                  --
---------------------------------------------------------------------------------------------------
import qualified Data.Char as C                    (toUpper)
import qualified Data.Maybe as M                   (fromMaybe)
import qualified Graphics.X11.ExtraTypes.XF86 as X
import qualified System.Environment as E           (getEnv, getEnvironment)

import XMonad
import qualified XMonad.Actions.CopyWindow as CW  (kill1, copy)    -- dwm-style windows.
import qualified XMonad.Actions.CycleWS as CWS    (prevWS, nextWS, shiftToPrev, shiftToNext)
import qualified XMonad.Actions.GridSelect as GS  (defaultGSConfig, goToSelected)
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Hooks.EwmhDesktops as ED  (ewmh, fullscreenEventHook)
import qualified XMonad.Hooks.ManageDocks as MD   (manageDocks, avoidStruts, docksEventHook)
import qualified XMonad.Hooks.ManageHelpers as MH (doCenterFloat, doFullFloat)
import qualified XMonad.Hooks.SetWMName as SWMN   (setWMName)      -- Java is bullshit.
import qualified XMonad.Layout.NoBorders as NB    (smartBorders)
import qualified XMonad.Layout.Spacing as S       (smartSpacing)
import qualified XMonad.StackSet as SSet          (view, shift)    -- Extra workspaces.
import qualified XMonad.Util.EZConfig as EZConfig (additionalKeys)
import XMonad.Util.Run
---------------------------------------------------------------------------------------------------
-------------------------------------------  MAIN  ------------------------------------------------
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Get environment to pull some XDG vars.
    env <- E.getEnvironment

    let dzenLeftSize = 1000
    let trayerWidth = 200
    let fullWidth = 1920
    let dzenRightSize = fullWidth - dzenLeftSize - trayerWidth

    dzenL <- spawnPipe $ dzenLeft dzenLeftSize
    dzenR <- spawnPipe $ dzenRight dzenLeftSize dzenRightSize (home ++ "/.config")
    trayr <- spawnPipe $ trayerCmd (dzenLeftSize + dzenRightSize) trayerWidth

    -- Config xmonad.
    xmonad $ ED.ewmh def
        { manageHook      = mHook <+> manageHook def
        , layoutHook      = MD.avoidStruts $ NB.smartBorders $ layoutHook def
        , startupHook     = SWMN.setWMName "LG3D" -- Prevents Java apps being grey blobs.
        , logHook         = loHook dzenL
        , handleEventHook = ED.fullscreenEventHook <+> MD.docksEventHook <+> handleEventHook def

        , modMask    = mod4Mask
        , workspaces = spaces
        , terminal   = term

        , focusFollowsMouse = False -- Mice are for squares.
        , clickJustFocuses  = False -- Focusing click passed to window.

        , focusedBorderColor = "#C0C5CE"
        , normalBorderColor  = "#65737E"
        , borderWidth        = 2
        } `EZConfig.additionalKeys` keybinds env mod4Mask

---------------------------------------------------------------------------------------------------
-------------------------------------------  KEYBINDS  --------------------------------------------
---------------------------------------------------------------------------------------------------
-- Keybinds, depending on host for audio keys.
keybinds env mask =
    -- Killing a window only removes it from current workspace.
    [ ((mask .|. shiftMask, xK_c), CW.kill1)

    -- Cycle/shift/gridselect workspaces.
    , ((mask .|. controlMask,               xK_h), CWS.prevWS)
    , ((mask .|. controlMask,               xK_l), CWS.nextWS)
    , ((mask .|. controlMask .|. shiftMask, xK_h), CWS.shiftToPrev)
    , ((mask .|. controlMask .|. shiftMask, xK_l), CWS.shiftToNext)
    , ((mask,                               xK_x), GS.goToSelected def)

    -- Brightness, screensaver.
    , ((0,                  X.xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                  X.xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")
    , ((mask .|. shiftMask, xK_l),                       spawn "xscreensaver-command -lock")

    , ((mask, xK_p), spawn dmenuRunStylized)

    -- Restart.
    , ((mask, xK_q), spawn myRestart)

    -- Screenshots.
    , ((0,           xK_Print), spawn $ scrot picHome "")
    , ((controlMask, xK_Print), spawn $ scrot picHome "select")
    , ((shiftMask,   xK_Print), spawn $ scrot picHome "delay 5")

    -- Quick program spawns.
    , ((mask,                 xK_a),              spawn $ cmd "alsamixer")
    , ((mask,                 xK_o),              spawn $ cmd "ncmpcpp")
    , ((mask,                 xK_u),              spawn $ cmd "watch acpi | less")
    , ((0,                    X.xF86XK_HomePage), spawn "x-www-browser")
    , ((mask .|. controlMask, xK_a),              spawn $ cmd "ssh-add")

    -- Various useful scripts, which are also in my config repo.
    -- TODO provide function to wrap 'menu <thing> <dir> dmenu_cmd'.
    , ((mask,               xK_s), spawn $ binHome ++ "set_wallpaper " ++ paperHome)
    , ((mask .|. shiftMask, xK_s), spawn $ unwords [binHome ++ "set_wallpaper", paperHome, "shuf"])
    , ((mask,               xK_n), spawn $ binHome ++ "toggle_oneko")
    , ((mask .|. shiftMask, xK_p), spawn $ "export DISPLAY=':0.0' " ++ menu "pass" passHome)
    , ((mask,               xK_v), spawn $ menu "vid" vidHome)
    , ((mask .|. shiftMask, xK_o), spawn $ menu "music" musicHome)
    ] ++ audioKeys ++

    -- mod-<N> switches to workspace N.
    -- mod-shift-<N> moves window to workspace N.
    -- mod-control-shift-<N> copies window to workspace N.
    [((mask .|. m, k), windows $ f i) | (i, k) <- zip spaces wsKeys
                                      , (f, m) <- [(SSet.view,  0),
                                                   (SSet.shift, shiftMask),
                                                   (CW.copy,    shiftMask .|. controlMask)]]

        where wsKeys  = xK_grave : [xK_1..xK_9] ++ [xK_0, xK_minus, xK_equal]

              -- Determine where various things are stored.
              picHome   = M.fromMaybe (home ++ "/pictures") (lookup "XDG_PICTURES_DIR" env)
              vidHome   = M.fromMaybe (home ++ "/videos")   (lookup "XDG_VIDEOS_DIR" env)
              musicHome = M.fromMaybe (home ++ "/music")    (lookup "XDG_MUSIC_DIR" env)

              dataHome = M.fromMaybe (home ++ "/.local/share") (lookup "XDG_DATA_HOME" env)

              binHome  = dataHome ++ "/bin/"
              passHome = dataHome ++ "/password-store"

              paperHome = picHome ++ "/wallpapers"

              menu      = menuer binHome

---------------------------------------------------------------------------------------------------
--------------------------------------------  OTHER  ----------------------------------------------
---------------------------------------------------------------------------------------------------
-- Terminal commands.
term  = "TERM=xterm-256color; mate-terminal"
cmd c = term ++ " --title=" ++ name ++ " --command='" ++ c ++ "'"
    where name = "__" ++ map C.toUpper (head (words c))

-- Workspace titles.  This looked cooler than just a list of strings..
spaces :: [String]
spaces = zipWith (++) ("`" : map show [1..9] ++ ["0", "-", "="])
                      (" term" : concatMap (replicate 3) [" socl", " play", " work", " etc."])

audioKeys = [ ((shiftMask, X.xF86XK_AudioMute),        spawn "mpc toggle")
            , ((shiftMask, X.xF86XK_AudioRaiseVolume), spawn "mpc next")
            , ((shiftMask, X.xF86XK_AudioLowerVolume), spawn "mpc prev")
            , ((0,         X.xF86XK_AudioPlay),        spawn "mpc toggle")
            , ((0,         X.xF86XK_AudioNext),        spawn "mpc next")
            , ((0,         X.xF86XK_AudioPrev),        spawn "mpc prev")
            , ((0,         X.xF86XK_AudioMute),        spawn "/usr/bin/pulseaudio-ctl mute")
            , ((0,         X.xF86XK_AudioLowerVolume), spawn "/usr/bin/pulseaudio-ctl down")
            , ((0,         X.xF86XK_AudioRaiseVolume), spawn "/usr/bin/pulseaudio-ctl up")]

-- Previously mentioned screenshotting nonsense.
scrot :: String -> String -> String
scrot picHome cmd = unwords ["sleep", "0.2;", "scrot", destination, cmdArg]
    where format      = "%FT%TZ%z.png" -- ISO 8601
          destination = picHome ++ "/screenshots/" ++ format
          cmdArg      = if cmd == "" then "" else "--" ++ cmd

-- Dmenu-based menu script.
menuer :: String -> (String -> String -> String)
menuer binHome thing home = unwords [binHome ++ "menu", thing, home, dmenuCmd]
    where dmenuCmd = "\"dmenu -i -fn " ++ font 32 ++ "\""

-- For some reason, xmonad has trouble getting the "$HOME" var. So, set a home var manually.
home :: String
home = "/home/amichaud"

---------------------------------------------------------------------------------------------------
-----------------------------------  STATUS BAR STUFF ---------------------------------------------
---------------------------------------------------------------------------------------------------
-- Left Dzen - xmonad info and window title.
dzenLeft :: Int -> String
dzenLeft width = "dzen2 -dock -x '0' -w '" ++ show width ++ "' -ta 'l'" ++ dzenStyle

-- Right Dzen - Runs conky config.
dzenRight :: Int -> Int -> String -> String
dzenRight start width config = conkyCmd ++ " | dzen2 " ++ style
    where conkyCmd = "conky --config=" ++ config ++ "/conky/dzen_config.lua"
          style    = "-x '" ++ show start ++ "' -w '" ++ show width ++ "' -ta 'r'" ++ dzenStyle

-- Dzen style
dzenStyle = " -fn " ++ font 16 ++ " -h '" ++ show statusHeight ++ "' -y '0' -bg '" ++
            normalBG ++ "' -fg '" ++ normalFG ++ "'"

trayerCmd start width = "trayer --widthtype pixel --width " ++ show width ++
                        " --edge top --SetDockType true" ++
                        " --heighttype pixel --height " ++ show statusHeight ++
                        " --distance " ++ show start ++ " --distancefrom left" ++
                        " --align left"

-- Fonts
font size = "DejaVuSansMono:" ++ show size

-- XMonad colors
-- :: String
normalFG = "#C0C5CE"
normalBG = "#2B303B"
currentBG = "#C0C5CE"
currentFG = "#2B303B"
urgentBG = "#BF616A"

-- Status bar height, trayer width.
statusHeight = 32
trayerWidth = 70

-- Pretty printing.
myDzenPP handle = DL.dzenPP
    { DL.ppCurrent = DL.dzenColor currentFG currentBG . DL.pad
    , DL.ppHidden  = DL.dzenColor normalFG normalBG . DL.pad . take 1
    , DL.ppOutput  = hPutStrLn handle
    , DL.ppUrgent  = DL.dzenColor currentFG urgentBG . DL.pad
    , DL.ppSep     = " "
    , DL.ppTitle   = DL.dzenColor normalFG normalBG . DL.pad
    , DL.ppLayout  = take 1}

dmenuRunStylized = "dmenu_run " ++
                   " -fn " ++ font 16 ++
                   " -nb '" ++ normalBG ++ "' -nf '" ++ normalFG ++
                   "' -sb '" ++ currentBG ++ "' -sf '" ++ currentFG ++ "'"

---------------------------------------------------------------------------------------------------
---------------------------------------------CUSTOM HOOKS------------------------------------------
---------------------------------------------------------------------------------------------------
-- Manage docks, custom layout nonsense.
-- TODO make a general rule somehow
mHook :: ManageHook
mHook = MD.manageDocks <+> composeAll
    [ title     =? "__NCMPCPP"   --> MH.doCenterFloat
    , title     =? "__ALSAMIXER" --> MH.doCenterFloat
    , title     =? "__XMONADHS"  --> MH.doCenterFloat
    , title     =? "__SSH-ADD"   --> MH.doCenterFloat
    , title     =? "__ACPI"      --> MH.doCenterFloat

    -- Handle gimp and mpv specially.
    , className =? "Gimp" --> doFloat
    , className =? "mpv"  --> MH.doFullFloat]

loHook h = DL.dynamicLogWithPP $ myDzenPP h

--https://wiki.haskell.org/Xmonad/Config_archive/Regalia's_xmonad.hs
-- Kill zombie dzens before normal xmonad restart
myRestart :: String
myRestart = "xmonad --recompile" ++
            "&& for pid in `pgrep dzen2`; do kill $pid; done" ++
            "&& for pid in `pgrep dzen2`; do kill -9 $pid; done" ++
            "&& for pid in `pgrep trayer`; do kill $pid; done" ++
            "&& for pid in `pgrep trayer`; do kill -9 $pid; done" ++
            "&& xmonad --restart"
