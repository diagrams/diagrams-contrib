{-# LANGUAGE LambdaCase #-}

import           Control.Arrow
import           Control.Lens
import           Data.Char
import           Data.List.Split
import qualified Data.Map        as M

data ColorDef = ColorDef { colorName :: String, colorValue :: String }

main = interact process
  where
    process = generateDefs . map processColor . tail . lines
    processColor = mkColorDef . (init &&& last) . words

    mkColorDef :: ([String], String) -> ColorDef
    mkColorDef = ColorDef <$> (mkVarName . fst) <*> snd
    mkVarName (n:ns)
      = map (\case '/' -> '_'; c -> c)
      $ n ++ concatMap upFirst ns
    upFirst (c:cs)   = toUpper c : cs

    generateDefs :: [ColorDef] -> String
    generateDefs cs = formatColorDefs cs ++ "\n" ++ formatColorMap cs

    formatColorDefs cs = unlines . map mkEqn $ cs
      where
        width = maximum . map (length . colorName) $ cs
        mkEqn (ColorDef v c) = padded v ++ " = fromJust $ readHexColor " ++ show c
        padded a = a ++ (replicate (width - length a) ' ')

    formatColorMap cs =
      unlines $
        [ "xkcdColorMap :: M.Map String (AlphaColour Double)"
        , "xkcdColorMap = M.fromList"
        ]
        ++
        (map mkAssoc cs & _Cons . _2 . traverse %~ ("  , "++)
                        & _Cons . _1            %~ ("  [ "++)
        )
        ++
        [ "  ]" ]

      where
        mkAssoc (ColorDef n c) = "(" ++ show n ++ ", " ++ n ++ ")"
