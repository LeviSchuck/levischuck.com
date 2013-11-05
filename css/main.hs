{-# LANGUAGE OverloadedStrings #-}
import           Clay
import qualified Data.Text.Lazy.IO as T
import           Data.Monoid
import           Control.Monad

main :: IO ()
main = T.putStr $ renderWith compact $ do
    let basetextcolor = color "#404040"
        secondcolor = color "#555555"
    a ? do
        color "#43557F"
    body ? do
        width $ px 600
        sym2 margin 0 auto
        backgroundColor "#F7F7F7"
        basetextcolor
        backgroundImage $ url "/images/dots.svg"
        fontSize $ pct 100
        "line-height" -: "1.25"
    "#header" ? do
        borderBottom solid (px 2) black
        sym2 padding (px 12) 0
        marginBottom $ px 30
        "#navigation" ? do
            textAlign $ alignSide sideRight
            a ? do
                basetextcolor
                fontSize $ px 18
                fontWeight bold
                marginLeft $ px 12
                textDecoration none
                textTransform uppercase

    "#logo" ? a ? do
        basetextcolor
        float floatLeft
        fontSize $ px 18
        fontWeight bold
        textDecoration none
    "#footer" ? do
        borderTop solid (px 2) black
        secondcolor
        fontSize $ px 12
        marginTop $ px 30
        sym2 padding (px 12) 0
        textAlign $ alignSide sideRight
    ".info" ? do
        secondcolor
        fontSize $ px 14
        fontStyle italic
    (h1 <> h2 <> h3) ? do
        marginTop 0
        marginBottom 0
        color "#222A40"
    let modular = [(h1, 2.625, 1.27976), (h2, 2, 1.25), (h3, 1.5, 1.45833)]
    forM_ modular $ \(tag, size, pad) -> tag ? do
        fontSize $ em size
        sym2 padding (em pad) 0


