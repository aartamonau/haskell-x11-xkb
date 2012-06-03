{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.X11.Xkb where

import Foreign
import Foreign.C

import Control.Exception ( assert )

import qualified Graphics.X11 as X11

#include <X11/XKBlib.h>

type OpcodeMajor  = #{type int}
type OpcodeMinor  = #{type int}
type EventBase    = #{type int}
type ErrorBase    = #{type int}
type VersionMajor = #{type int}
type VersionMinor = #{type int}

versionMajor :: VersionMajor
versionMajor = #{const XkbMajorVersion}

versionMinor :: VersionMinor
versionMinor = #{const XkbMinorVersion}

versionString :: String
versionString = show versionMajor ++ "." ++ show versionMinor

foreign import ccall unsafe "XkbLibraryVersion"
  xkbLibraryVersion :: Ptr VersionMajor -> Ptr VersionMinor -> IO Bool

libraryVersion :: IO (Bool, VersionMajor, VersionMinor)
libraryVersion =
  alloca $ \p_versionMajor ->
  alloca $ \p_versionMinor -> do
    poke p_versionMajor versionMajor
    poke p_versionMinor versionMinor

    compatible <- xkbLibraryVersion p_versionMajor p_versionMinor

    libVersionMajor <- peek p_versionMajor
    libVersionMinor <- peek p_versionMinor

    return (compatible, libVersionMajor, libVersionMinor)

foreign import ccall unsafe "XkbQueryExtension"
  xkbQueryExtension :: X11.Display
                    -> Ptr OpcodeMajor
                    -> Ptr EventBase
                    -> Ptr ErrorBase
                    -> Ptr VersionMajor
                    -> Ptr VersionMinor
                    -> IO Bool

queryExtension :: X11.Display
               -> VersionMajor
               -> VersionMinor
               -> IO (Either (VersionMajor, VersionMinor)
                             (OpcodeMajor, EventBase, ErrorBase))
queryExtension dpy versionMajor versionMinor =
  alloca $ \p_opcodeMajor ->
  alloca $ \p_eventBase ->
  alloca $ \p_errorBase ->
  alloca $ \p_versionMajor ->
  alloca $ \p_versionMinor -> do
    poke p_versionMajor versionMajor
    poke p_versionMinor versionMinor

    ret <- xkbQueryExtension dpy p_opcodeMajor p_eventBase p_errorBase
                             p_versionMajor p_versionMinor

    if ret
      then do
        opcodeMajor <- peek p_opcodeMajor
        eventBase   <- peek p_eventBase
        errorBase   <- peek p_errorBase

        return $ Right (opcodeMajor, eventBase, errorBase)
      else do
        serverMajor <- peek p_versionMajor
        serverMinor <- peek p_versionMinor

        return $ Left (serverMajor, serverMinor)

type XkbOpenDisplayStatus = #{type int}
#{enum XkbOpenDisplayStatus,
 , xkb_OD_Success           = XkbOD_Success
 , xkb_OD_BadLibraryVersion = XkbOD_BadLibraryVersion
 , xkb_OD_ConnectionRefused = XkbOD_ConnectionRefused
 , xkb_OD_NonXkbServer      = XkbOD_NonXkbServer
 , xkb_OD_BadServerVersion  = XkbOD_BadServerVersion
 }

foreign import ccall unsafe "XkbOpenDisplay"
  xkbOpenDisplay :: CString
                 -> Ptr EventBase
                 -> Ptr ErrorBase
                 -> Ptr VersionMajor
                 -> Ptr VersionMinor
                 -> Ptr XkbOpenDisplayStatus
                 -> IO (Ptr X11.Display)

openDisplay :: String -> IO (EventBase, ErrorBase, X11.Display)
openDisplay name =
  withCString name $ \c_name ->
  alloca $ \p_eventBase ->
  alloca $ \p_errorBase ->
  alloca $ \p_versionMajor ->
  alloca $ \p_versionMinor ->
  alloca $ \p_status -> do
      poke p_versionMajor versionMajor
      poke p_versionMinor versionMinor

      dpy <- xkbOpenDisplay c_name p_eventBase p_errorBase
                            p_versionMajor p_versionMinor p_status

      status <- peek p_status
      maybeThrow dpy status

      eventBase <- peek p_eventBase
      errorBase <- peek p_errorBase

      return (eventBase, errorBase, X11.Display dpy)

  where maybeThrow :: Ptr X11.Display -> XkbOpenDisplayStatus -> IO ()
        maybeThrow dpy status
          | status == xkb_OD_Success = assert (dpy /= nullPtr) $ return ()
          | otherwise =
              assert (dpy == nullPtr) $ throw (errorMsg status)

        throw :: String -> IO ()
        throw = ioError . userError

        errorMsg :: XkbOpenDisplayStatus -> String
        errorMsg status
          | status == xkb_OD_BadLibraryVersion =
            "xlib of incompatible version with ours: " ++ versionString
          | status == xkb_OD_BadServerVersion =
            "xserver of incompatible version with ours: " ++ versionString
          | status == xkb_OD_ConnectionRefused = "failed to open display"
          | status == xkb_OD_NonXkbServer = "XKB extension not present"

foreign import ccall unsafe "XkbIgnoreExtension"
  ignoreExtension :: Bool -> IO Bool

type XkbEventType = #{type int}
#{enum XkbEventType,
  , xkb_NewKeyboardNotify     = XkbNewKeyboardNotify
  , xkb_MapNotify             = XkbMapNotify
  , xkb_StateNotify           = XkbStateNotify
  , xkb_ControlsNotify        = XkbControlsNotify
  , xkb_IndicatorStateNotify  = XkbIndicatorStateNotify
  , xkb_IndicatorMapNotify    = XkbIndicatorMapNotify
  , xkb_NamesNotify           = XkbNamesNotify
  , xkb_CompatMapNotify       = XkbCompatMapNotify
  , xkb_BellNotify            = XkbBellNotify
  , xkb_ActionMessage         = XkbActionMessage
  , xkb_AccessXNotify         = XkbAccessXNotify
  , xkb_ExtensionDeviceNotify = XkbExtensionDeviceNotify
  }

newtype XkbEvent = XkbEvent XkbEventPtr
type XkbEventPtr = Ptr XkbEvent
type XkbEventMask = #{type unsigned long int}

type XkbDevice = #{type unsigned int}
#{enum XkbDevice,
 , xkb_UseCoreKbd   = XkbUseCoreKbd
 , xkb_UseCorePtr   = XkbUseCorePtr
 , xkb_DfltXIClass  = XkbDfltXIClass
 , xkb_DfltXIId     = XkbDfltXIId
 , xkb_AllXIClasses = XkbAllXIClasses
 , xkb_AllXIIds     = XkbAllXIIds
 , xkb_XINone       = XkbXINone
 }

get_XkbEventType :: XkbEventPtr -> IO XkbEventType
get_XkbEventType = #{peek XkbAnyEvent, xkb_type}

get_XkbDevice :: XkbEventPtr -> IO XkbDevice
get_XkbDevice = #{peek XkbAnyEvent, device}

get_Time :: XkbEventPtr -> IO X11.Time
get_Time = #{peek XkbAnyEvent, time}


foreign import ccall unsafe "XkbSelectEvents"
  selectEvents :: X11.Display
               -> XkbDevice
               -> XkbEventMask
               -> XkbEventMask
               -> IO Bool

type XkbStateNotifyEventDetailMask = #{type int}
#{enum XkbStateNotifyEventDetailMask,
  , xkb_ModifierStateMask      = XkbModifierStateMask
  , xkb_ModifierBaseMask       = XkbModifierBaseMask
  , xkb_ModifierLatchMask      = XkbModifierLatchMask
  , xkb_ModifierLockMask       = XkbModifierLockMask
  , xkb_GroupStateMask         = XkbGroupStateMask
  , xkb_GroupBaseMask          = XkbGroupBaseMask
  , xkb_GroupLatchMask         = XkbGroupLatchMask
  , xkb_GroupLockMask          = XkbGroupLockMask
  , xkb_CompatStateMask        = XkbCompatStateMask
  , xkb_GrabModsMask           = XkbGrabModsMask
  , xkb_CompatGrabModsMask     = XkbCompatGrabModsMask
  , xkb_LookupModsMask         = XkbLookupModsMask
  , xkb_CompatLookupModsMask   = XkbCompatLookupModsMask
  , xkb_PointerButtonMask      = XkbPointerButtonMask
  , xkb_AllStateComponentsMask = XkbAllStateComponentsMask
  }

type XkbEventDetailMask = #{type unsigned long int}

foreign import ccall unsafe "XkbSelectEventDetails"
  selectEventDetails :: X11.Display
                     -> XkbDevice
                     -> XkbEventType
                     -> XkbEventDetailMask
                     -> XkbEventDetailMask
                     -> IO Bool

type XkbGroup = #{type int}
#{enum XkbGroup,
 , xkb_Group1Index = XkbGroup1Index
 , xkb_Group2Index = XkbGroup2Index
 , xkb_Group3Index = XkbGroup3Index
 , xkb_Group4Index = XkbGroup4Index
 }

foreign import ccall unsafe "XkbLockGroup"
  lockGroup :: X11.Display
            -> XkbDevice
            -> XkbGroup
            -> IO Bool

foreign import ccall unsafe "XkbLatchGroup"
  latchGroup :: X11.Display
             -> XkbDevice
             -> XkbGroup
             -> IO Bool

foreign import ccall unsafe "XkbLockModifiers"
  lockModifiers :: X11.Display
                -> XkbDevice
                -> X11.KeyMask
                -> X11.KeyMask
                -> IO Bool

foreign import ccall unsafe "XkbLatchModifiers"
  latchModifiers :: X11.Display
                 -> XkbDevice
                 -> X11.KeyMask
                 -> X11.KeyMask
                 -> IO Bool

type XkbState =
  ( XkbGroup                      -- group
  , XkbGroup                      -- base_group
  , XkbGroup                      -- latched_group
  , XkbGroup                      -- locked_group
  , X11.KeyMask                   -- mods
  , X11.KeyMask                   -- base_mods
  , X11.KeyMask                   -- latched_mods
  , X11.KeyMask                   -- locked_mods
  , X11.KeyMask                   -- compat_state
  , X11.KeyMask                   -- grab_mods
  , X11.KeyMask                   -- compat_grab_mods
  , X11.KeyMask                   -- lookup_mods
  , X11.KeyMask                   -- compat_lookup_mods
  , X11.ButtonMask                -- ptr_buttons
  )

allocaXkbState :: (Ptr XkbState -> IO a) -> IO a
allocaXkbState = allocaBytes #{size XkbStateRec}

peekXkbState :: Ptr XkbState -> IO XkbState
peekXkbState p = do
  group              <- #{peek XkbStateRec, group} p
  base_group         <- #{peek XkbStateRec, base_group} p
  latched_group      <- #{peek XkbStateRec, latched_group} p
  locked_group       <- #{peek XkbStateRec, locked_group} p
  mods               <- #{peek XkbStateRec, mods} p
  base_mods          <- #{peek XkbStateRec, base_mods} p
  latched_mods       <- #{peek XkbStateRec, latched_mods} p
  locked_mods        <- #{peek XkbStateRec, locked_mods} p
  compat_state       <- #{peek XkbStateRec, compat_state} p
  grab_mods          <- #{peek XkbStateRec, grab_mods} p
  compat_grab_mods   <- #{peek XkbStateRec, compat_grab_mods} p
  lookup_mods        <- #{peek XkbStateRec, lookup_mods} p
  compat_lookup_mods <- #{peek XkbStateRec, compat_lookup_mods} p
  ptr_buttons        <- #{peek XkbStateRec, ptr_buttons} p

  return (group,
          base_group,
          latched_group,
          locked_group,
          mods,
          base_mods,
          latched_mods,
          locked_mods,
          compat_state,
          grab_mods,
          compat_grab_mods,
          lookup_mods,
          compat_lookup_mods,
          ptr_buttons)

foreign import ccall unsafe "XkbGetState"
  xkbGetState :: X11.Display
              -> XkbDevice
              -> Ptr XkbState
              -> IO X11.Status

getState :: X11.Display -> XkbDevice -> IO (Maybe XkbState)
getState dpy device =
  allocaXkbState $ \p_xkbState -> do
    status <- xkbGetState dpy device p_xkbState
    if status == 0
      then fmap Just (peekXkbState p_xkbState)
      else return Nothing

type XkbStateNotifyEvent =
  ( XkbStateNotifyEventDetailMask -- changed
  , XkbGroup                      -- group
  , XkbGroup                      -- base_group
  , XkbGroup                      -- latched_group
  , XkbGroup                      -- locked_group
  , X11.KeyMask                   -- mods
  , X11.KeyMask                   -- base_mods
  , X11.KeyMask                   -- latched_mods
  , X11.KeyMask                   -- locked_mods
  , X11.KeyMask                   -- compat_state
  , X11.KeyMask                   -- grab_mods
  , X11.KeyMask                   -- compat_grab_mods
  , X11.KeyMask                   -- lookup_mods
  , X11.KeyMask                   -- compat_lookup_mods
  , X11.ButtonMask                -- ptr_buttons
  , X11.KeyCode                   -- keycode
  , X11.EventType                 -- event_type
  , OpcodeMajor                   -- req_major
  , OpcodeMinor                   -- req_minor
  )

allocaXkbStateNotifyEvent :: (Ptr XkbStateNotifyEvent -> IO a) -> IO a
allocaXkbStateNotifyEvent = allocaBytes #{size XkbStateNotifyEvent}

peekXkbStateNotifyEvent :: Ptr XkbStateNotifyEvent
                           -> IO XkbStateNotifyEvent
peekXkbStateNotifyEvent p = do
  changed            <- #{peek XkbStateNotifyEvent, changed} p
  group              <- #{peek XkbStateNotifyEvent, group} p
  base_group         <- #{peek XkbStateNotifyEvent, base_group} p
  latched_group      <- #{peek XkbStateNotifyEvent, latched_group} p
  locked_group       <- #{peek XkbStateNotifyEvent, locked_group} p
  mods               <- #{peek XkbStateNotifyEvent, mods} p
  base_mods          <- #{peek XkbStateNotifyEvent, base_mods} p
  latched_mods       <- #{peek XkbStateNotifyEvent, latched_mods} p
  locked_mods        <- #{peek XkbStateNotifyEvent, locked_mods} p
  compat_state       <- #{peek XkbStateNotifyEvent, compat_state} p
  grab_mods          <- #{peek XkbStateNotifyEvent, grab_mods} p
  compat_grab_mods   <- #{peek XkbStateNotifyEvent, compat_grab_mods} p
  lookup_mods        <- #{peek XkbStateNotifyEvent, lookup_mods} p
  compat_lookup_mods <- #{peek XkbStateNotifyEvent, compat_lookup_mods} p
  ptr_buttons        <- #{peek XkbStateNotifyEvent, ptr_buttons} p
  keycode            <- #{peek XkbStateNotifyEvent, keycode} p
  event_type         <- #{peek XkbStateNotifyEvent, event_type} p
  req_major          <- #{peek XkbStateNotifyEvent, req_major} p
  req_minor          <- #{peek XkbStateNotifyEvent, req_minor} p

  return (changed,
          group,
          base_group,
          latched_group,
          locked_group,
          mods,
          base_mods,
          latched_mods,
          locked_mods,
          compat_state,
          grab_mods,
          compat_grab_mods,
          lookup_mods,
          compat_lookup_mods,
          ptr_buttons,
          keycode,
          event_type,
          req_major,
          req_minor)

get_XkbStateNotifyEvent :: XkbEventPtr -> IO XkbStateNotifyEvent
get_XkbStateNotifyEvent p = peekXkbStateNotifyEvent (castPtr p)
