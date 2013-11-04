{-# LANGUAGE OverloadedStrings #-}
import           Clay
import qualified Data.Text.Lazy.IO as T
import           Data.Monoid

inch :: Double -> Size Abs
inch x = pt $ inch' x

inch' :: Double -> Double
inch' x = 72.0 * x


main :: IO ()
main = T.putStr $ renderWith compact $ do
    let w = inch' 7.5
    body' w
    h1'
    h2'
    dt' w
    dd' w
    a'
    hr'
    blockquote'
    p'

body' :: Double -> Css
body' w = body ? do
    width $ pt w
    fontFamily ["Helvetica Neue", "Helvetica", "Arial"] [sansSerif]
    sym margin auto
    background white
    sym padding (inch 0.5)

smallShadow :: Css
smallShadow = textShadow (px 1) (px 1) (px 1) (rgb 51 51 51)

h1' :: Css
h1' = h1 ? do
    let hcolor = rgb 117 117 117
    fontSize (inch 0.6)
    color hcolor
    textAlign $ alignSide sideCenter
    marginBottom $ inch 0.25
    hover & do
        color white
        backgroundColor hcolor
        smallShadow

h2' :: Css
h2' = h2 ? do
    color $ rgb 77 123 145
    before & do
        content $ stringContent ""
        display inlineBlock
        marginRight $ pt 6
        width $ pct 16
        height $ inch 0.1
        backgroundColor $ rgb 127 205 242
    hover & do
        backgroundColor $ rgb 107 173 204
        color white
        smallShadow

dt' :: Double -> Css
dt' w = dt ? do
    float floatLeft
    clear clearLeft
    width $ pt $ 0.17 * w

dd' :: Double -> Css
dd' w = dd ? do
    textAlign justify
    paddingBottom $ em 1
    marginLeft $ pt $ 0.17 * w

a' :: Css
a' = a ? do
    textDecoration none
    color $ rgb 77 123 145
    let hovactive = do
        color white
        backgroundColor $ rgb 107 173 204
        textDecoration none
        smallShadow
    hover & hovactive
    active & hovactive

hr' = hr ? do
    color $ rgb 166 166 166

blockquote' :: Css
blockquote' = blockquote ? do
    textAlign $ alignSide sideCenter
p' :: Css
p' = p ? do
    marginTop 0
    marginBottom $ px 7
