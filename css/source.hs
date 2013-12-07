{-# LANGUAGE OverloadedStrings #-}
import           Clay
import qualified Data.Text.Lazy.IO as T
import           Data.Monoid
import           Control.Monad

main :: IO ()
main = putCss $ do
	pre # ".sourceCode" ? do
        ".kw" ? do
            color "#F92672" -- Keyword
            fontWeight bold
        ".dt" ? do
            color "#66D9EF" -- Type color
        ".dv" ? color "#AE81FF" -- Number
        ".bn" ? color "#40a070"
        ".fl" ? color "#40a070"
        ".ch" ? color "#40a070"
        ".st" ? color "#E6DB74" -- String
        ".co" ? do
            color "#75715E"
            fontStyle italic
        ".ot" ? color "#A6E22E" -- function name in type decl
        ".fu" ? color "#F92672" -- function call
        -- ".re"
        ".err" ? do
            color red
            fontWeight bold
        ".al" ? do
            color red
            fontWeight bold
        padding (px 20) (px 20) (px 20) (px 20)
        borderRadius (px 2) (px 2) (px 2) (px 2)
        backgroundColor "#272822"
        color white
		