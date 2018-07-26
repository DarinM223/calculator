{-# LANGUAGE OverloadedLabels #-}

module Main where

import Calculator
import Control.Monad
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.IORef
import Data.Maybe
import TextShow

import qualified Data.Text as T
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
    Gtk.init Nothing

    builder <- Gtk.builderNew
    Gtk.builderAddFromFile builder "window.glade"

    window <- Gtk.builderGetObject builder "window1"
          >>= unsafeCastTo Gtk.Window . fromJust

    output <- Gtk.builderGetObject builder "output"
          >>= unsafeCastTo Gtk.Entry . fromJust

    ref <- newIORef newCalculator :: IO (IORef (Calculator Double))
    let buttonFromId = unsafeCastTo Gtk.Button
                     . fromJust
                   <=< Gtk.builderGetObject builder
    numButtons buttonFromId >>= \buttons -> forM_ buttons $ \(num, button) ->
        Gtk.onButtonClicked button $
            runAndSetOutput ref output (pressNumber num)

    opsButtons buttonFromId >>= \buttons -> forM_ buttons $ \(op, button) ->
        Gtk.onButtonClicked button $ runAndSetOutput ref output (setOp op)

    buttonFromId "buttonClear" >>= flip Gtk.onButtonClicked
        (runAndSetOutput ref output (const newCalculator))
    buttonFromId "buttonEquals" >>= flip Gtk.onButtonClicked
        (runAndSetOutput ref output evaluate)
    buttonFromId "buttonDelete" >>= flip Gtk.onButtonClicked
        (runAndSetOutput ref output deleteNumber)

    Gtk.onWidgetDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.main

runAndSetOutput :: (TextShow n, RealFrac n)
                => IORef (Calculator n)
                -> Gtk.Entry
                -> (Calculator n -> Calculator n)
                -> IO ()
runAndSetOutput ref output f = do
    c <- readIORef ref
    let c' = f c
    writeIORef ref c'
    Gtk.entrySetText output (displayNumber $ _number c')

displayNumber :: (TextShow n, RealFrac n) => n -> T.Text
displayNumber num = if floor num == ceiling num
    then showt $ (truncate num :: Int)
    else showt num

opsTable :: [(Operator, T.Text)]
opsTable = [ (Plus, "buttonPlus")
           , (Minus, "buttonMinus")
           , (Times, "buttonTimes")
           , (Div, "buttonDiv")
           ]

opsButtons :: (T.Text -> IO Gtk.Button) -> IO [(Operator, Gtk.Button)]
opsButtons buttonFromId = mapM toButton opsTable
  where
    toButton (op, id) = (,) <$> pure op <*> buttonFromId id

numButtons :: (Num n, Enum n)
           => (T.Text -> IO Gtk.Button)
           -> IO [(n, Gtk.Button)]
numButtons buttonFromId = zip <$> pure [0..] <*> buttons
  where
    buttonNums = [0..9] :: [Int]
    buttons = mapM (buttonFromId . T.append "button" . showt) buttonNums
