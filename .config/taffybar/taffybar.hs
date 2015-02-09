import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager (wrap, colorize, shorten, escape)
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS2

-------------------
----- COLORS ------
-------------------

base03  = "#002b36"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

main = do
  let clock  = textClockNew Nothing "<span fgcolor='#fdf6e3'>%_d %b %Y|%A|%H:%M</span>" 1
      pager  = pagerConfig
      tray   = systrayNew
      mpris2 = mpris2New
  defaultTaffybar defaultTaffybarConfig { startWidgets = [pager]
                                        , endWidgets = [ tray, clock, mpris2]
                                        , barHeight = 16
                                        }

--------------------------------------------------------------------------------------------
------------------------------------   FUNCTIONS   -----------------------------------------
--------------------------------------------------------------------------------------------

-- Pager config.
pagerConfig = taffyPagerNew defaultPagerConfig
                { emptyWorkspace   = const ""
                , activeWorkspace  = colorize base03 orange . wrap "[" "]"
                , visibleWorkspace = colorize base03 cyan . wrap "(" ")"
                , urgentWorkspace  = colorize base03 red . wrap "<" ">"
                , hiddenWorkspace  = take 1
                , widgetSep = " "
                , activeLayout = take 1
                , activeWindow = shorten 70 . escape
                }
