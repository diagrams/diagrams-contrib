{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.Swimunit.Dotmatrix
where

import qualified Data.Map as Map

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

{-|
  Renders list of characters to a diagram where the characters
  are shown as if displayed by an old dotmatrix panel.
  Only the light emitting dots are shown.

  This function maps a list of n characters passed as third argument
  to n dotmatrix characters.
 -}
dotmatrix  :: Diagram B                        -- ^ Diagram to show a 'dot'.
           -> Dotfont                          -- ^ The Dotfont to be used.
           -> String                           -- ^ List of characters to be rendered in dotmatrix.
           -> Diagram B                        -- ^ Resulting diagram.
dotmatrix  dt fnt txt = dotmatrix' dt fnt False txt

{-|
  Renders list of characters to a diagram where the characters
  are shown as if displayed by an old dotmatrix panel.
  Only the dark dots are shown. This function is usually used to render
  the back panel.

  Function is calling dotmatrix.
 -}
notdotmatrix :: Diagram B                      -- ^ Diagram to show a 'dot'.
             -> Dotfont                        -- ^ The Dotfont to be used.
             -> String                         -- ^ List of characters to be rendered in dotmatrix.
             -> Diagram B                      -- ^ Resulting diagram.
notdotmatrix dt fnt txt = dotmatrix' dt fnt True txt

{-|
  Renders list of characters to a diagram where the characters
  are shown as if displayed by an old dotmatrix panel.
  Whether light emitting dots or backpanel dots are shown is controlled by third
  argument.

  This function maps a list of n characters passed as fourth argument
  to n dotmatrix characters. The third argument decides whether the
  dots of the font or the dots of the backplane are rendered.

  Devnote: Function loops over list of characters in fourth argument.
 -}
dotmatrix' :: Diagram B                       -- ^ Diagram to show a 'dot'.
           -> Dotfont                         -- ^ The Dotfont to be used.
           -> Bool                            -- ^ True if inverse of font is returned.
           -> String                          -- ^ List of characters to be rendered in dotmatrix.
           -> Diagram B                       -- ^ Resulting diagram.
dotmatrix' dt fnt rev txt = hcat (map (dotcharblock dt fnt rev) txt)

{-|
  Vertically stacks a number of lines of dots prepared by function dotline
  to a block of dots representing a Dotchar.
 -}
dotcharblock :: Diagram B                       -- ^ Diagram to show a 'dot'.
             -> Dotfont                         -- ^ The Dotfont to be used.
             -> Bool                            -- ^ If true comparison is inverted.
             -> Char                            -- ^ Character to be mapped to a Dotchar.
             -> Diagram B                       -- ^ Resulting diagram.
dotcharblock dt fnt rev c = vcat (map (dotcharline dt rev)
                                      (dotcharlistoflistsofchars(dotcharmap fnt c))
                                 )

{-|
  Maps sequence of Char to a horizontal sequence of dots.
  Devnote: Due to partial application the line has to be the last argument.
 -}
dotcharline :: Diagram B                      -- ^ Diagram to show a 'dot'.
            -> Bool                           -- ^ If true comparison is inverted.
            -> [Char]                         -- ^ Squence of characters.
            -> Diagram B                      -- ^ Resulting diagram.
dotcharline dt rev ln = hcat (map (dotornotdot dt rev) ln)

{-|
  Maps a Char to a Dotchar.
  If Char has no associated entry the undefined Dotchar is returned. Note that
  therefore the mapping to the undefined character has always to be present.
 -}
dotcharmap :: Dotfont                         -- ^ The Dotfont to be used.
           -> Char                            -- ^ Character to be mapped to a bunch of dots.
           -> Dotchar                         -- ^ Resulting dot matrix character.
dotcharmap fnt c = case (Map.lookup c fnt) of
                       Just x -> x
                       Nothing -> case (Map.lookup '\0' fnt) of
                                      Just y -> y
                                      Nothing -> Dotchar [[]]



{-|
  Function decides whether a character (passed as third argument)
  in a character definition is interpreted as a dot or not.
  If it is a dot the diagram passed as first argument is rendered
  otherwise the phantom of the diagram is returned.

  To make the rendering of the back panel easier the second argument
  can invert the decision.

  Devnote: Due to partial application of this function the character deciding
           must be the last argument.
 -}
dotornotdot :: Diagram B                      -- ^ Diagram to show a 'dot'.
            -> Bool                           -- ^ If true comparison is inverted.
            -> Char                           -- ^ Character to control generation of 'dot' or empty space ('notdot').
            -> Diagram B                      -- ^ Resulting diagram.
dotornotdot dt rev c =
            if rev
                then
                    if c == ' '               -- add more conditions here
                        then dt
                        else (phantom dt)
                else
                    if c /= ' '               -- add more conditions here
                        then dt
                        else (phantom dt)

{-|
  A character for Dotmatrix consists of a number of lines of characters.
  All characters for a given character set should have the same width and height.
 -}
data Dotchar = Dotchar {
                   dotcharlistoflistsofchars :: [[Char]]
               }

{-|
  Defines a font for Dotmatrix which is nothing more than a collection
  of Dotcharacters with a map than combines ASCII characters to Dotcharacters.
 -}
type Dotfont = Map.Map Char Dotchar


{-|
  This is the concrete mapping of Char to Dotchar for font B6x9.
 -}
dotfont_B6x9 :: Map.Map Char Dotchar
dotfont_B6x9 = Map.fromList ([
                 (' '     , dotchar_Blank_B6x9)
               , ('#'     , dotchar_Hash_B6x9)
               , (':'     , dotchar_Colon_B6x9)
               , (','     , dotchar_Comma_B6x9)
               , (';'     , dotchar_Semicolon_B6x9)
               , ('.'     , dotchar_Point_B6x9)
               , ('+'     , dotchar_Plus_B6x9)
               , ('-'     , dotchar_Minus_B6x9)
               , ('_'     , dotchar_Underscore_B6x9)
               , ('!'     , dotchar_Exclamation_B6x9)
               , ('='     , dotchar_Equals_B6x9)
               , ('0'     , dotchar_0__B6x9)
               , ('1'     , dotchar_1__B6x9)
               , ('2'     , dotchar_2__B6x9)
               , ('3'     , dotchar_3__B6x9)
               , ('4'     , dotchar_4__B6x9)
               , ('5'     , dotchar_5__B6x9)
               , ('6'     , dotchar_6__B6x9)
               , ('7'     , dotchar_7__B6x9)
               , ('8'     , dotchar_8__B6x9)
               , ('9'     , dotchar_9__B6x9)
               , ('A'     , dotchar_A__B6x9)
               , ('a'     , dotchar_A__B6x9)
               , ('B'     , dotchar_B__B6x9)
               , ('b'     , dotchar_B__B6x9)
               , ('C'     , dotchar_C__B6x9)
               , ('c'     , dotchar_C__B6x9)
               , ('D'     , dotchar_D__B6x9)
               , ('d'     , dotchar_D__B6x9)
               , ('E'     , dotchar_E__B6x9)
               , ('e'     , dotchar_E__B6x9)
               , ('F'     , dotchar_F__B6x9)
               , ('f'     , dotchar_F__B6x9)
               , ('G'     , dotchar_G__B6x9)
               , ('g'     , dotchar_G__B6x9)
               , ('H'     , dotchar_H__B6x9)
               , ('h'     , dotchar_H__B6x9)
               , ('I'     , dotchar_I__B6x9)
               , ('i'     , dotchar_I__B6x9)
               , ('J'     , dotchar_J__B6x9)
               , ('j'     , dotchar_J__B6x9)
               , ('K'     , dotchar_K__B6x9)
               , ('k'     , dotchar_K__B6x9)
               , ('L'     , dotchar_L__B6x9)
               , ('l'     , dotchar_L__B6x9)
               , ('M'     , dotchar_M__B6x9)
               , ('m'     , dotchar_M__B6x9)
               , ('N'     , dotchar_N__B6x9)
               , ('n'     , dotchar_N__B6x9)
               , ('O'     , dotchar_O__B6x9)
               , ('o'     , dotchar_O__B6x9)
               , ('P'     , dotchar_P__B6x9)
               , ('p'     , dotchar_P__B6x9)
               , ('Q'     , dotchar_Q__B6x9)
               , ('q'     , dotchar_Q__B6x9)
               , ('R'     , dotchar_R__B6x9)
               , ('r'     , dotchar_R__B6x9)
               , ('S'     , dotchar_S__B6x9)
               , ('s'     , dotchar_S__B6x9)
               , ('T'     , dotchar_T__B6x9)
               , ('t'     , dotchar_T__B6x9)
               , ('U'     , dotchar_U__B6x9)
               , ('u'     , dotchar_U__B6x9)
               , ('V'     , dotchar_V__B6x9)
               , ('v'     , dotchar_V__B6x9)
               , ('W'     , dotchar_W__B6x9)
               , ('w'     , dotchar_W__B6x9)
               , ('X'     , dotchar_X__B6x9)
               , ('x'     , dotchar_X__B6x9)
               , ('Y'     , dotchar_Y__B6x9)
               , ('y'     , dotchar_Y__B6x9)
               , ('Z'     , dotchar_Z__B6x9)
               , ('z'     , dotchar_Z__B6x9)
               , ('\0'    , dotchar_Unmapped_B6x9) -- Only mapping that unconditionally has to be present.
               ])

dotchar_Hash_B6x9 :: Dotchar
dotchar_Hash_B6x9 = Dotchar
              ["      "
              ," # #  "
              ," # #  "
              ,"##### "
              ," # #  "
              ,"##### "
              ," # #  "
              ," # #  "
              ,"      "]

dotchar_Blank_B6x9 :: Dotchar
dotchar_Blank_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "]

dotchar_Colon_B6x9 :: Dotchar
dotchar_Colon_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"  ::  "
              ,"  ::  "
              ,"      "
              ,"  ::  "
              ,"  ::  "
              ,"      "]

dotchar_Comma_B6x9 :: Dotchar
dotchar_Comma_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"  ,,  "
              ,"  ,,  "
              ,"  ,,  "
              ," ,,   "]

dotchar_Semicolon_B6x9 :: Dotchar
dotchar_Semicolon_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"  ;;  "
              ,"      "
              ,"  ;;  "
              ,"  ;;  "
              ,"  ;;  "
              ," ;;   "]

dotchar_Point_B6x9 :: Dotchar
dotchar_Point_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"  ..  "
              ,"  ..  "
              ,"      "]

dotchar_Plus_B6x9 :: Dotchar
dotchar_Plus_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"  +   "
              ,"  +   "
              ,"+++++ "
              ,"  +   "
              ,"  +   "
              ,"      "
              ,"      "]

dotchar_Minus_B6x9 :: Dotchar
dotchar_Minus_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"      "
              ,"----- "
              ,"      "
              ,"      "
              ,"      "
              ,"      "]

dotchar_Equals_B6x9 :: Dotchar
dotchar_Equals_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"===== "
              ,"      "
              ,"===== "
              ,"      "
              ,"      "
              ,"      "]

dotchar_Underscore_B6x9 :: Dotchar
dotchar_Underscore_B6x9 = Dotchar
              ["      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"      "
              ,"_____ "]

dotchar_Exclamation_B6x9 :: Dotchar
dotchar_Exclamation_B6x9 = Dotchar
              ["      "
              ,"  !   "
              ,"  !   "
              ,"  !   "
              ,"  !   "
              ,"  !   "
              ,"      "
              ,"  !   "
              ,"      "]

dotchar_0__B6x9 :: Dotchar
dotchar_0__B6x9 = Dotchar
              ["      "
              ," 000  "
              ,"0   0 "
              ,"00  0 "
              ,"0 0 0 "
              ,"0  00 "
              ,"0   0 "
              ," 000  "
              ,"      "]

dotchar_1__B6x9 :: Dotchar
dotchar_1__B6x9 = Dotchar
              ["      "
              ,"  1   "
              ," 11   "
              ,"1 1   "
              ,"  1   "
              ,"  1   "
              ,"  1   "
              ,"1111  "
              ,"      "]

dotchar_2__B6x9 :: Dotchar
dotchar_2__B6x9 = Dotchar
              ["      "
              ," 222  "
              ,"2   2 "
              ,"    2 "
              ,"   2  "
              ,"  2   "
              ," 2    "
              ,"22222 "
              ,"      "]

dotchar_3__B6x9 :: Dotchar
dotchar_3__B6x9 = Dotchar
              ["      "
              ,"33333 "
              ,"   3  "
              ,"  3   "
              ," 333  "
              ,"    3 "
              ,"    3 "
              ,"3333  "
              ,"      "]

dotchar_4__B6x9 :: Dotchar
dotchar_4__B6x9 = Dotchar
              ["      "
              ,"   44 "
              ,"  4 4 "
              ," 4  4 "
              ,"44444 "
              ,"    4 "
              ,"    4 "
              ,"    4 "
              ,"      "]

dotchar_5__B6x9 :: Dotchar
dotchar_5__B6x9 = Dotchar
              ["      "
              ,"55555 "
              ,"5     "
              ,"5     "
              ,"5555  "
              ,"    5 "
              ,"5   5 "
              ," 555  "
              ,"      "]

dotchar_6__B6x9 :: Dotchar
dotchar_6__B6x9 = Dotchar
              ["      "
              ," 666  "
              ,"6     "
              ,"6     "
              ,"6666  "
              ,"6   6 "
              ,"6   6 "
              ," 666  "
              ,"      "]

dotchar_7__B6x9 :: Dotchar
dotchar_7__B6x9 = Dotchar
              ["      "
              ,"77777 "
              ,"    7 "
              ,"   7  "
              ,"  7   "
              ," 7    "
              ," 7    "
              ," 7    "
              ,"      "]

dotchar_8__B6x9 :: Dotchar
dotchar_8__B6x9 = Dotchar
              ["      "
              ," 888  "
              ,"8   8 "
              ,"8   8 "
              ," 888  "
              ,"8   8 "
              ,"8   8 "
              ," 888  "
              ,"      "]

dotchar_9__B6x9 :: Dotchar
dotchar_9__B6x9 = Dotchar
              ["      "
              ," 999  "
              ,"9   9 "
              ,"9   9 "
              ," 9999 "
              ,"    9 "
              ,"    9 "
              ," 999  "
              ,"      "]

dotchar_A__B6x9 :: Dotchar
dotchar_A__B6x9 = Dotchar
              ["      "
              ," AAA  "
              ,"A   A "
              ,"A   A "
              ,"AAAAA "
              ,"A   A "
              ,"A   A "
              ,"A   A "
              ,"      "]


{-
dotchar_AE_B6x9 :: Dotchar
dotchar_AE_B6x9 = Dotchar
              [" Ä Ä  "
              ,"      "
              ," ÄÄÄ  "
              ,"Ä   Ä "
              ,"Ä   Ä "
              ,"ÄÄÄÄÄ "
              ,"Ä   Ä "
              ,"Ä   Ä "
              ,"      "]
-}

dotchar_B__B6x9 :: Dotchar
dotchar_B__B6x9 = Dotchar
              ["      "
              ,"BBBB  "
              ," B  B "
              ," B  B "
              ," BBB  "
              ," B  B "
              ," B  B "
              ,"BBBB  "
              ,"      "]

dotchar_C__B6x9 :: Dotchar
dotchar_C__B6x9 = Dotchar
              ["      "
              ," CCC  "
              ,"C   C "
              ,"C     "
              ,"C     "
              ,"C     "
              ,"C   C "
              ," CCC  "
              ,"      "]

dotchar_D__B6x9 :: Dotchar
dotchar_D__B6x9 = Dotchar
              ["      "
              ,"DDDD  "
              ," D  C "
              ," D  D "
              ," D  D "
              ," D  D "
              ," D  D "
              ,"DDDD  "
              ,"      "]

dotchar_E__B6x9 :: Dotchar
dotchar_E__B6x9 = Dotchar
              ["      "
              ,"EEEEE "
              ,"E     "
              ,"E     "
              ,"EEEE  "
              ,"E     "
              ,"E     "
              ,"EEEEE "
              ,"      "]

dotchar_F__B6x9 :: Dotchar
dotchar_F__B6x9 = Dotchar
              ["      "
              ,"FFFFF "
              ,"F     "
              ,"F     "
              ,"FFFF  "
              ,"F     "
              ,"F     "
              ,"F     "
              ,"      "]

dotchar_G__B6x9 :: Dotchar
dotchar_G__B6x9 = Dotchar
              ["      "
              ," GGGG "
              ,"G     "
              ,"G     "
              ,"G 66G "
              ,"G   G "
              ,"G   G "
              ," GGG  "
              ,"      "]

dotchar_H__B6x9 :: Dotchar
dotchar_H__B6x9 = Dotchar
              ["      "
              ,"H   H "
              ,"H   H "
              ,"H   H "
              ,"HHHHH "
              ,"H   H "
              ,"H   H "
              ,"H   H "
              ,"      "]

dotchar_I__B6x9 :: Dotchar
dotchar_I__B6x9 = Dotchar
              ["      "
              ," III  "
              ,"  I   "
              ,"  I   "
              ,"  I   "
              ,"  I   "
              ,"  I   "
              ," III  "
              ,"      "]

dotchar_J__B6x9 :: Dotchar
dotchar_J__B6x9 = Dotchar
              ["      "
              ," JJJ  "
              ,"   J  "
              ,"   J  "
              ,"   J  "
              ,"   J  "
              ,"J  J  "
              ," JJ   "
              ,"      "]

dotchar_K__B6x9 :: Dotchar
dotchar_K__B6x9 = Dotchar
              ["      "
              ,"K   K "
              ,"K  K  "
              ,"K K   "
              ,"KK    "
              ,"K K   "
              ,"K  K  "
              ,"K   K "
              ,"      "]

dotchar_L__B6x9 :: Dotchar
dotchar_L__B6x9 = Dotchar
              ["      "
              ,"L     "
              ,"L     "
              ,"L     "
              ,"L     "
              ,"L     "
              ,"L     "
              ,"LLLLL "
              ,"      "]


dotchar_M__B6x9 :: Dotchar
dotchar_M__B6x9 = Dotchar
              ["      "
              ,"M   M "
              ,"MM MM "
              ,"MM MM "
              ,"W M M "
              ,"M   M "
              ,"M   M "
              ,"M   M "
              ,"      "]

dotchar_N__B6x9 :: Dotchar
dotchar_N__B6x9 = Dotchar
              ["      "
              ,"N   N "
              ,"N   N "
              ,"NN  N "
              ,"N N N "
              ,"N  NN "
              ,"N  NN "
              ,"N   N "
              ,"      "]

dotchar_O__B6x9 :: Dotchar
dotchar_O__B6x9 = Dotchar
              ["      "
              ," OOO  "
              ,"O   O "
              ,"O   O "
              ,"O   O "
              ,"O   O "
              ,"O   O "
              ," OOO  "
              ,"      "]


{-
dotchar_OE_B6x9 :: Dotchar
dotchar_OE_B6x9 = Dotchar
              [" Ö Ö  "
              ,"      "
              ," ÖÖÖ  "
              ,"Ö   Ö "
              ,"Ö   Ö "
              ,"Ö   Ö "
              ,"Ö   Ö "
              ," ÖÖÖ  "
              ,"      "]
-}

dotchar_P__B6x9 :: Dotchar
dotchar_P__B6x9 = Dotchar
              ["      "
              ,"PPPP  "
              ,"P   P "
              ,"P   P "
              ,"PPPP  "
              ,"P     "
              ,"P     "
              ,"P     "
              ,"      "]

dotchar_Q__B6x9 :: Dotchar
dotchar_Q__B6x9 = Dotchar
              ["      "
              ," QQQ  "
              ,"Q   Q "
              ,"Q   Q "
              ,"Q   Q "
              ,"Q Q Q "
              ,"Q  QQ "
              ," QQQQ "
              ,"     Q"]

dotchar_R__B6x9 :: Dotchar
dotchar_R__B6x9 = Dotchar
              ["      "
              ,"RRRR  "
              ,"R   R "
              ,"R   R "
              ,"RRRR  "
              ,"R  R  "
              ,"R   R "
              ,"R   R "
              ,"      "]

dotchar_S__B6x9 :: Dotchar
dotchar_S__B6x9 = Dotchar
              ["      "
              ," SSS  "
              ,"S   S "
              ,"S     "
              ," SSS  "
              ,"    S "
              ,"S   S "
              ," SSS  "
              ,"      "]

dotchar_T__B6x9 :: Dotchar
dotchar_T__B6x9 = Dotchar
              ["      "
              ,"TTTTT "
              ,"  T   "
              ,"  T   "
              ,"  T   "
              ,"  T   "
              ,"  T   "
              ,"  T   "
              ,"      "]

dotchar_U__B6x9 :: Dotchar
dotchar_U__B6x9 = Dotchar
              ["      "
              ,"U   U "
              ,"U   U "
              ,"U   U "
              ,"U   U "
              ,"U   U "
              ,"U   U "
              ," UUU  "
              ,"      "]

{-
dotchar_UE_B6x9 :: Dotchar
dotchar_UE_B6x9 = Dotchar
              [" Ü Ü  "
              ,"      "
              ,"Ü   Ü "
              ,"Ü   Ü "
              ,"Ü   Ü "
              ,"Ü   Ü "
              ,"Ü   Ü "
              ," ÜÜÜ  "
              ,"      "]
-}

dotchar_V__B6x9 :: Dotchar
dotchar_V__B6x9 = Dotchar
              ["      "
              ,"V   V "
              ,"V   V "
              ,"V   V "
              ,"V   V "
              ," V V  "
              ," V V  "
              ,"  V   "
              ,"      "]

dotchar_W__B6x9 :: Dotchar
dotchar_W__B6x9 = Dotchar
              ["      "
              ,"W   W "
              ,"W   W "
              ,"W   W "
              ,"W W W "
              ,"W W W "
              ,"WW WW "
              ,"W   W "
              ,"      "]

dotchar_X__B6x9 :: Dotchar
dotchar_X__B6x9 = Dotchar
              ["      "
              ,"X   X "
              ,"X   X "
              ," X X  "
              ,"  X   "
              ," X X  "
              ,"X   X "
              ,"X   X "
              ,"      "]

dotchar_Y__B6x9 :: Dotchar
dotchar_Y__B6x9 = Dotchar
              ["      "
              ,"Y   Y "
              ,"Y   Y "
              ," Y Y  "
              ,"  Y   "
              ,"  Y   "
              ,"  Y   "
              ,"  Y   "
              ,"      "]

dotchar_Z__B6x9 :: Dotchar
dotchar_Z__B6x9 = Dotchar
              ["      "
              ,"ZZZZZ "
              ,"    Z "
              ,"   Z  "
              ,"  Z   "
              ," Z    "
              ,"Z     "
              ,"ZZZZZ "
              ,"      "]

dotchar_Unmapped_B6x9 :: Dotchar
dotchar_Unmapped_B6x9 = Dotchar
              ["      "
              ,"+ + + "
              ," + +  "
              ,"+ + + "
              ," + +  "
              ,"+ + + "
              ," + +  "
              ,"+ + + "
              ,"      "]

 --
----
 --
