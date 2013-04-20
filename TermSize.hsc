{-# LANGUAGE ForeignFunctionInterface #-}

-- I get this module from:
--   http://stackoverflow.com/questions/12806053/get-terminal-width-haskell

module TermSize (getTermSize) where

import Foreign
import Foreign.C.Error (throwErrno)
import Foreign.C.Types

#include <sys/ioctl.h>
#include <unistd.h>

-- Trick for calculating alignment of a type, taken from
-- http://www.haskell.org/haskellwiki/FFICookBook#Working_with_structs
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- The ws_xpixel and ws_ypixel fields are unused, so I've omitted them here.
data WinSize = WinSize { wsRow, wsCol :: CUShort }

instance Storable WinSize where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize) 
  peek ptr = do
    row <- (#peek struct winsize, ws_row) ptr
    col <- (#peek struct winsize, ws_col) ptr
    return $ WinSize row col
  poke ptr (WinSize row col) = do
    (#poke struct winsize, ws_row) ptr row
    (#poke struct winsize, ws_col) ptr col

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt

defaultErrno :: Maybe a -> String -> IO a
defaultErrno (Just x) _ = return x
defaultErrno Nothing s = throwErrno s

-- | Return current number of (rows, columns) of the terminal.
getTermSize :: Maybe (Int, Int) -> IO (Int, Int)
getTermSize defaultSize = 
  with (WinSize 0 0) $ \ws -> do
    rc <- ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) ws
    WinSize row col <- peek ws
    if rc == CInt (-1)
      then defaultErrno defaultSize "ioctl"
      else return (fromIntegral row, fromIntegral col)
