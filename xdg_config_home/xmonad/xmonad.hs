---------------------------------------------------------------------------------------------------
-- AUTHOR:  Andrew Michaud                                                                       --
--          https://andrewmichaud.com                                                            --
-- FILE:    xmonad.hs                                                                            --
-- PURPOSE: XMonad configuration file                                                            --
-- UPDATED: 2015-08-30                                                                           --
-- LICENSE: MIT/BSD                                                                              --
---------------------------------------------------------------------------------------------------
import qualified Data.Char as C                         (toUpper)
import qualified Data.Maybe as M                        (fromMaybe)
import qualified Graphics.X11.ExtraTypes.XF86 as X
import qualified System.Environment as E                (getEnvironment)
import qualified System.Taffybar.Hooks.PagerHints as PH (pagerHints)

import XMonad
import qualified XMonad.Actions.CopyWindow as CW  (kill1, copy)    -- dwm-style windows.
import qualified XMonad.Actions.CycleWS as CWS    (prevWS, nextWS, shiftToPrev, shiftToNext)
import qualified XMonad.Actions.GridSelect as GS  (defaultGSConfig, goToSelected)
import qualified XMonad.Hooks.EwmhDesktops as ED  ( ewmhDesktopsLogHook
                                                  , ewmhDesktopsEventHook
                                                  , ewmhDesktopsStartup
                                                  , fullscreenEventHook)
import qualified XMonad.Hooks.ManageDocks as MD   (manageDocks, avoidStruts)
import qualified XMonad.Hooks.ManageHelpers as MH (doCenterFloat, doFullFloat)
import qualified XMonad.Hooks.SetWMName as SWMN   (setWMName)      -- Java is bullshit.
import qualified XMonad.Layout.NoBorders as NB    (smartBorders)
import qualified XMonad.StackSet as SSet          (view, shift)    -- Extra workspaces.
import qualified XMonad.Util.EZConfig as EZConfig (additionalKeys)

import qualified Colors as CL

---------------------------------------------------------------------------------------------------
-------------------------------------------  MAIN  ------------------------------------------------
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Get environment to pull some XDG vars.
    env <- E.getEnvironment

    -- Config xmonad.
    xmonad $ PH.pagerHints $ defaultConfig
        { manageHook = mHook <+> manageHook defaultConfig
        , layoutHook = lHook

        -- Handles Java, ewmh nonsense.
        , startupHook = ED.ewmhDesktopsStartup <+> SWMN.setWMName "LG3D"

        , logHook         = ED.ewmhDesktopsLogHook
        , handleEventHook = ED.ewmhDesktopsEventHook <+> ED.fullscreenEventHook <+>
                            handleEventHook defaultConfig

        , modMask    = mod4Mask
        , workspaces = spaces
        , terminal   = term

        , focusFollowsMouse = False -- Mice are for squares.
        , clickJustFocuses  = False -- Focusing click passed to window.

        -- Border jazz.
        , focusedBorderColor = CL.base2
        , normalBorderColor  = CL.base03
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
    , ((mask,                               xK_x), GS.goToSelected GS.defaultGSConfig)

    -- Commands.  Brightness, screensaver, bar restart.
    , ((0,                    X.xF86XK_MonBrightnessDown), spawn "xbacklight -dec 7")
    , ((0,                    X.xF86XK_MonBrightnessUp),   spawn "xbacklight -inc 7")
    , ((mask .|. shiftMask,   xK_l),                       spawn "xscreensaver-command -lock")
    , ((mask .|. controlMask, xK_q),                       spawn "pkill taffybar && taffybar")

    -- Screenshots.
    , ((0,           xK_Print), spawn $ scrot picHome "")
    , ((controlMask, xK_Print), spawn $ scrot picHome "select")
    , ((shiftMask,   xK_Print), spawn $ scrot picHome "delay 5")

    -- Quick program spawns.
    , ((mask,                 xK_a), spawn $ cmd "alsamixer")
    , ((mask,                 xK_o), spawn $ cmd "ncmpcpp")
    , ((mask,                 xK_b), spawn "x-www-browser")
    , ((mask .|. controlMask, xK_a), spawn $ cmd "ssh-add")

    -- Various useful scripts, which are also in my config repo.
    , ((mask,               xK_s), spawn $ binHome ++ "setWallpaper")
    , ((mask,               xK_n), spawn $ binHome ++ "toggleOneko")
    , ((mask .|. shiftMask, xK_p), spawn $ binHome ++ "menu pass")
    , ((mask,               xK_v), spawn $ binHome ++ "menu vid")
    , ((mask .|. shiftMask, xK_o), spawn $ binHome ++ "menu music")
    ] ++ audioKeys ++

    -- mod-<N> switches to workspace N.
    -- mod-shift-<N> moves window to workspace N.
    -- mod-control-shift-<N> copies window to workspace N.
    [((mask .|. m, k), windows $ f i) | (i, k) <- zip spaces wsKeys
                                      , (f, m) <- [(SSet.view,  0),
                                                   (SSet.shift, shiftMask),
                                                   (CW.copy,    shiftMask .|. controlMask)]]

        where wsKeys  = xK_grave : [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]

              -- Determine xdg_data_home to grab scripts correctly.
              -- You may store your scripts elsewhere and want to change this.
              binHome = M.fromMaybe "~/.local/share" (lookup "XDG_DATA_HOME"    env) ++ "/bin"
              picHome = M.fromMaybe "~/pictures"     (lookup "XDG_PICTURES_DIR" env)

---------------------------------------------------------------------------------------------------
--------------------------------------------  OTHER  ----------------------------------------------
---------------------------------------------------------------------------------------------------
-- Terminal commands.
term  = "xfce4-terminal --hide-menubar --show-borders"
cmd c = term ++ " --title=" ++ name ++ " --command='" ++ c ++ "'"
    where name = "__" ++ map C.toUpper (head (words c))

-- Workspace titles.  This looked cooler than just a list of strings..
spaces :: [String]
spaces = zipWith (++) ("`" : map show [1..9] ++ ["0", "-", "="])
                      (" term" : concatMap (replicate 3) [" socl", " play", " work", " etc."])

-- Laptop doesn't have proper media keys because Lenovo are dumb.  So, do it manually.
-- Actually, why not define those silly media keys for any host?  No downside.
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
scrot picHome cmd = unwords ["scrot",format, destination, cmd]
          -- Year, month, day, width by height.
    where format       = "'%Y-%m-%d-%s_$wx$h.png'"
          destination  = "-e 'mv $f " ++ picHome ++ "/screenshots/'"

---------------------------------------------------------------------------------------------------
------------------------------------------  CUSTOM HOOKS  -----------------------------------------
---------------------------------------------------------------------------------------------------
-- Manage docks, custom layout nonsense.
mHook :: ManageHook
mHook = MD.manageDocks <+> composeAll
    [ title     =? "__NCMPCPP"   --> MH.doCenterFloat
    , title     =? "__ALSAMIXER" --> MH.doCenterFloat
    , title     =? "__XMONADHS"  --> MH.doCenterFloat
    , title     =? "__SSH-ADD"   --> MH.doCenterFloat

    -- Handle gimp and mpv specially.
    , className =? "Gimp" --> doFloat
    , className =? "mpv"  --> MH.doFullFloat

    -- Chat windows go to workspace 3
    , className =? "Pidgin" --> doShift "3:chat"
    ]

lHook = MD.avoidStruts $ NB.smartBorders layouts
    where layouts = tiled ||| Mirror tiled ||| Full
          tiled   = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 1/2
          delta   = 3/100
