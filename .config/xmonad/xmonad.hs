import XMonad
import XMonad.Config
import XMonad.ManageHook
import XMonad.Operations
import XMonad.Actions.NoBorders
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.Accordion
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap,mkKeymap)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Font
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86

myTerminal :: String
myTerminal = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = True


myBorderWidth = 1

myModMask = mod4Mask

myNormalBorderColor :: String
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor :: String
myFocusedBorderColor = "#FAD32F"

data FocusedOnly = FocusedOnly
  deriving (Show, Read)

instance SetsAmbiguous FocusedOnly where
  hiddens :: FocusedOnly -> WindowSet -> Rectangle -> Maybe (W.Stack Window) -> [(Window, Rectangle)] -> [Window]
  hiddens _ wset _ _ wrs =
    case W.peek wset of
      Nothing -> fmap fst wrs
      Just focused -> filter (/= focused) $ fmap fst wrs

myWorkspaces = [" \xf120  ", " \xf0ac  ", " \xee9c  ", " \xeda9  ", " \xef1e  ", " \xf2dc  ", " \xf0b46  ", " \xf11eb  ", " \xefc8  "]

myStartupHook = do
           spawnOnce "udiskie &"

myKeys = [("M-<Return>", spawn myTerminal),
          ("M-q", kill),
          ("M-S-r", spawn "xmonad --recompile; xmonad --restart"),
          ("M-r", spawn "rofi -show run"),
          ("M-e", spawn "rofi -show emoji"),
          ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts),
          ("M-g", toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled),
          ("M-j", windows $ W.focusDown),
          ("M-k", windows $ W.focusUp),
          ("<XF86MonBrightnessUp>", spawn "lux -a 10%"),
          ("<XF86MonBrightnessDown>", spawn "lux -s 10%"),
          ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
          ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
          ("<XF86AudioMute>", spawn "amixer set Master toggle")
	  ]

myManageHooks = composeAll 
        [manageDocks,
	 className =? "firefox" --> doShift (myWorkspaces !! 1),
	 isFullscreen --> doFullFloat,
	 manageHook def]

myFadeHook = composeAll [opaque,
                         isUnfocused --> transparency 0.2
                        ]

myLayout = mkToggle (NBFULL ?? NOBORDERS ?? EOT) (spacingRaw False (Border 8 8 8 8) True (Border 8 8 8 8) True $ smartBorders $ lessBorders (FocusedOnly) $ ResizableTall 1 (3/100) (1/2) [] ||| Accordion)


main = do
         xbar <- spawnPipe "xmobar /home/matusu/.config/xmonad/.xmobarrc"
         xmonad $ xmobarProp $ def {
        terminal           = myTerminal,
        startupHook        = myStartupHook,
        manageHook         = myManageHooks,
        layoutHook         = myLayout,
        workspaces         = myWorkspaces,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        handleEventHook = fadeWindowsEventHook,
        logHook = dynamicLogWithPP xmobarPP {
         ppOutput = hPutStrLn xbar,
         ppCurrent = xmobarColor "#FAD32F" "" . wrap "<fc=#0A1124,#FAD32F> " " </fc>",
         ppVisible = xmobarColor "#98be65" "" . wrap " " " ",
         ppHidden = xmobarColor "#EB3247" "" . wrap " " " ",
         ppHiddenNoWindows = xmobarColor "#5F4149" "" . wrap " " " ",
         ppTitle = xmobarColor "#3afc2" "" . shorten 60,
         ppSep = "<fc=#666666> <fn=1>|</fn> </fc>",
         ppUrgent = xmobarColor "#C45500" "",
         ppExtras = [],
         ppOrder = \(ws:_) -> [ws]
         }
    } `additionalKeysP` myKeys
