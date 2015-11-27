-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Color.XKCD
-- Copyright   :  (c) 2015 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- The 949 most common RGB monitor colors, as determined by the xkcd
-- color name survey.  See http://xkcd.com/color/rgb/ .  Data taken
-- from http://xkcd.com/color/rgb.txt (License:
-- http://creativecommons.org/publicdomain/zero/1.0/ .)
--
-- This module defines many common color names that no standards
-- committee would ever countenance, including crude ones like
-- 'poopGreen' and 'puke', as well as more acceptable but still
-- non-standards-worthy names like 'windowsBlue' and 'toxicGreen'.  It
-- also contains several misspellings such as 'liliac' and 'manilla',
-- though by definition this list contains only very common
-- misspellings.
--
-- Names in the original list that use a forward slash
-- (e.g. "yellow/green") are represented here with an underscore,
-- e.g. 'yellow_green' (note that @yellow_green /= yellowGreen@!).
--
-- Note that the color name 'tan' clashes with the Haskell Prelude.
-- Many other color names here clash with names from
-- "Data.Colour.Names" (taken from the X11 standard), which is
-- automatically re-exported from "Diagrams.Prelude".  It is
-- recommended to import this module qualified.
-----------------------------------------------------------------------------

module Diagrams.Color.XKCD where

import           Data.Colour              (AlphaColour)
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import           Diagrams.Backend.CmdLine (readHexColor)
import           Prelude                  hiding (tan)

cloudyBlue            = fromJust $ readHexColor "#acc2d9"
darkPastelGreen       = fromJust $ readHexColor "#56ae57"
dust                  = fromJust $ readHexColor "#b2996e"
electricLime          = fromJust $ readHexColor "#a8ff04"
freshGreen            = fromJust $ readHexColor "#69d84f"
lightEggplant         = fromJust $ readHexColor "#894585"
nastyGreen            = fromJust $ readHexColor "#70b23f"
reallyLightBlue       = fromJust $ readHexColor "#d4ffff"
tea                   = fromJust $ readHexColor "#65ab7c"
warmPurple            = fromJust $ readHexColor "#952e8f"
yellowishTan          = fromJust $ readHexColor "#fcfc81"
cement                = fromJust $ readHexColor "#a5a391"
darkGrassGreen        = fromJust $ readHexColor "#388004"
dustyTeal             = fromJust $ readHexColor "#4c9085"
greyTeal              = fromJust $ readHexColor "#5e9b8a"
macaroniAndCheese     = fromJust $ readHexColor "#efb435"
pinkishTan            = fromJust $ readHexColor "#d99b82"
spruce                = fromJust $ readHexColor "#0a5f38"
strongBlue            = fromJust $ readHexColor "#0c06f7"
toxicGreen            = fromJust $ readHexColor "#61de2a"
windowsBlue           = fromJust $ readHexColor "#3778bf"
blueBlue              = fromJust $ readHexColor "#2242c7"
blueWithAHintOfPurple = fromJust $ readHexColor "#533cc6"
booger                = fromJust $ readHexColor "#9bb53c"
brightSeaGreen        = fromJust $ readHexColor "#05ffa6"
darkGreenBlue         = fromJust $ readHexColor "#1f6357"
deepTurquoise         = fromJust $ readHexColor "#017374"
greenTeal             = fromJust $ readHexColor "#0cb577"
strongPink            = fromJust $ readHexColor "#ff0789"
bland                 = fromJust $ readHexColor "#afa88b"
deepAqua              = fromJust $ readHexColor "#08787f"
lavenderPink          = fromJust $ readHexColor "#dd85d7"
lightMossGreen        = fromJust $ readHexColor "#a6c875"
lightSeafoamGreen     = fromJust $ readHexColor "#a7ffb5"
oliveYellow           = fromJust $ readHexColor "#c2b709"
pigPink               = fromJust $ readHexColor "#e78ea5"
deepLilac             = fromJust $ readHexColor "#966ebd"
desert                = fromJust $ readHexColor "#ccad60"
dustyLavender         = fromJust $ readHexColor "#ac86a8"
purpleyGrey           = fromJust $ readHexColor "#947e94"
purply                = fromJust $ readHexColor "#983fb2"
candyPink             = fromJust $ readHexColor "#ff63e9"
lightPastelGreen      = fromJust $ readHexColor "#b2fba5"
boringGreen           = fromJust $ readHexColor "#63b365"
kiwiGreen             = fromJust $ readHexColor "#8ee53f"
lightGreyGreen        = fromJust $ readHexColor "#b7e1a1"
orangePink            = fromJust $ readHexColor "#ff6f52"
teaGreen              = fromJust $ readHexColor "#bdf8a3"
veryLightBrown        = fromJust $ readHexColor "#d3b683"
eggShell              = fromJust $ readHexColor "#fffcc4"
eggplantPurple        = fromJust $ readHexColor "#430541"
powderPink            = fromJust $ readHexColor "#ffb2d0"
reddishGrey           = fromJust $ readHexColor "#997570"
babyShitBrown         = fromJust $ readHexColor "#ad900d"
liliac                = fromJust $ readHexColor "#c48efd"
stormyBlue            = fromJust $ readHexColor "#507b9c"
uglyBrown             = fromJust $ readHexColor "#7d7103"
custard               = fromJust $ readHexColor "#fffd78"
darkishPink           = fromJust $ readHexColor "#da467d"
deepBrown             = fromJust $ readHexColor "#410200"
greenishBeige         = fromJust $ readHexColor "#c9d179"
manilla               = fromJust $ readHexColor "#fffa86"
offBlue               = fromJust $ readHexColor "#5684ae"
battleshipGrey        = fromJust $ readHexColor "#6b7c85"
brownyGreen           = fromJust $ readHexColor "#6f6c0a"
bruise                = fromJust $ readHexColor "#7e4071"
kelleyGreen           = fromJust $ readHexColor "#009337"
sicklyYellow          = fromJust $ readHexColor "#d0e429"
sunnyYellow           = fromJust $ readHexColor "#fff917"
azul                  = fromJust $ readHexColor "#1d5dec"
darkgreen             = fromJust $ readHexColor "#054907"
green_yellow          = fromJust $ readHexColor "#b5ce08"
lichen                = fromJust $ readHexColor "#8fb67b"
lightLightGreen       = fromJust $ readHexColor "#c8ffb0"
paleGold              = fromJust $ readHexColor "#fdde6c"
sunYellow             = fromJust $ readHexColor "#ffdf22"
tanGreen              = fromJust $ readHexColor "#a9be70"
burple                = fromJust $ readHexColor "#6832e3"
butterscotch          = fromJust $ readHexColor "#fdb147"
toupe                 = fromJust $ readHexColor "#c7ac7d"
darkCream             = fromJust $ readHexColor "#fff39a"
indianRed             = fromJust $ readHexColor "#850e04"
lightLavendar         = fromJust $ readHexColor "#efc0fe"
poisonGreen           = fromJust $ readHexColor "#40fd14"
babyPukeGreen         = fromJust $ readHexColor "#b6c406"
brightYellowGreen     = fromJust $ readHexColor "#9dff00"
charcoalGrey          = fromJust $ readHexColor "#3c4142"
squash                = fromJust $ readHexColor "#f2ab15"
cinnamon              = fromJust $ readHexColor "#ac4f06"
lightPeaGreen         = fromJust $ readHexColor "#c4fe82"
radioactiveGreen      = fromJust $ readHexColor "#2cfa1f"
rawSienna             = fromJust $ readHexColor "#9a6200"
babyPurple            = fromJust $ readHexColor "#ca9bf7"
cocoa                 = fromJust $ readHexColor "#875f42"
lightRoyalBlue        = fromJust $ readHexColor "#3a2efe"
orangeish             = fromJust $ readHexColor "#fd8d49"
rustBrown             = fromJust $ readHexColor "#8b3103"
sandBrown             = fromJust $ readHexColor "#cba560"
swamp                 = fromJust $ readHexColor "#698339"
tealishGreen          = fromJust $ readHexColor "#0cdc73"
burntSiena            = fromJust $ readHexColor "#b75203"
camo                  = fromJust $ readHexColor "#7f8f4e"
duskBlue              = fromJust $ readHexColor "#26538d"
fern                  = fromJust $ readHexColor "#63a950"
oldRose               = fromJust $ readHexColor "#c87f89"
paleLightGreen        = fromJust $ readHexColor "#b1fc99"
peachyPink            = fromJust $ readHexColor "#ff9a8a"
rosyPink              = fromJust $ readHexColor "#f6688e"
lightBluishGreen      = fromJust $ readHexColor "#76fda8"
lightBrightGreen      = fromJust $ readHexColor "#53fe5c"
lightNeonGreen        = fromJust $ readHexColor "#4efd54"
lightSeafoam          = fromJust $ readHexColor "#a0febf"
tiffanyBlue           = fromJust $ readHexColor "#7bf2da"
washedOutGreen        = fromJust $ readHexColor "#bcf5a6"
brownyOrange          = fromJust $ readHexColor "#ca6b02"
niceBlue              = fromJust $ readHexColor "#107ab0"
sapphire              = fromJust $ readHexColor "#2138ab"
greyishTeal           = fromJust $ readHexColor "#719f91"
orangeyYellow         = fromJust $ readHexColor "#fdb915"
parchment             = fromJust $ readHexColor "#fefcaf"
straw                 = fromJust $ readHexColor "#fcf679"
veryDarkBrown         = fromJust $ readHexColor "#1d0200"
terracota             = fromJust $ readHexColor "#cb6843"
uglyBlue              = fromJust $ readHexColor "#31668a"
clearBlue             = fromJust $ readHexColor "#247afd"
creme                 = fromJust $ readHexColor "#ffffb6"
foamGreen             = fromJust $ readHexColor "#90fda9"
grey_green            = fromJust $ readHexColor "#86a17d"
lightGold             = fromJust $ readHexColor "#fddc5c"
seafoamBlue           = fromJust $ readHexColor "#78d1b6"
topaz                 = fromJust $ readHexColor "#13bbaf"
violetPink            = fromJust $ readHexColor "#fb5ffc"
wintergreen           = fromJust $ readHexColor "#20f986"
yellowTan             = fromJust $ readHexColor "#ffe36e"
darkFuchsia           = fromJust $ readHexColor "#9d0759"
indigoBlue            = fromJust $ readHexColor "#3a18b1"
lightYellowishGreen   = fromJust $ readHexColor "#c2ff89"
paleMagenta           = fromJust $ readHexColor "#d767ad"
richPurple            = fromJust $ readHexColor "#720058"
sunflowerYellow       = fromJust $ readHexColor "#ffda03"
green_blue            = fromJust $ readHexColor "#01c08d"
leather               = fromJust $ readHexColor "#ac7434"
racingGreen           = fromJust $ readHexColor "#014600"
vividPurple           = fromJust $ readHexColor "#9900fa"
darkRoyalBlue         = fromJust $ readHexColor "#02066f"
hazel                 = fromJust $ readHexColor "#8e7618"
mutedPink             = fromJust $ readHexColor "#d1768f"
boogerGreen           = fromJust $ readHexColor "#96b403"
canary                = fromJust $ readHexColor "#fdff63"
coolGrey              = fromJust $ readHexColor "#95a3a6"
darkTaupe             = fromJust $ readHexColor "#7f684e"
darkishPurple         = fromJust $ readHexColor "#751973"
trueGreen             = fromJust $ readHexColor "#089404"
coralPink             = fromJust $ readHexColor "#ff6163"
darkSage              = fromJust $ readHexColor "#598556"
darkSlateBlue         = fromJust $ readHexColor "#214761"
flatBlue              = fromJust $ readHexColor "#3c73a8"
mushroom              = fromJust $ readHexColor "#ba9e88"
richBlue              = fromJust $ readHexColor "#021bf9"
dirtyPurple           = fromJust $ readHexColor "#734a65"
greenblue             = fromJust $ readHexColor "#23c48b"
ickyGreen             = fromJust $ readHexColor "#8fae22"
lightKhaki            = fromJust $ readHexColor "#e6f2a2"
warmBlue              = fromJust $ readHexColor "#4b57db"
darkHotPink           = fromJust $ readHexColor "#d90166"
deepSeaBlue           = fromJust $ readHexColor "#015482"
carmine               = fromJust $ readHexColor "#9d0216"
darkYellowGreen       = fromJust $ readHexColor "#728f02"
palePeach             = fromJust $ readHexColor "#ffe5ad"
plumPurple            = fromJust $ readHexColor "#4e0550"
goldenRod             = fromJust $ readHexColor "#f9bc08"
neonRed               = fromJust $ readHexColor "#ff073a"
oldPink               = fromJust $ readHexColor "#c77986"
veryPaleBlue          = fromJust $ readHexColor "#d6fffe"
bloodOrange           = fromJust $ readHexColor "#fe4b03"
grapefruit            = fromJust $ readHexColor "#fd5956"
sandYellow            = fromJust $ readHexColor "#fce166"
clayBrown             = fromJust $ readHexColor "#b2713d"
darkBlueGrey          = fromJust $ readHexColor "#1f3b4d"
flatGreen             = fromJust $ readHexColor "#699d4c"
lightGreenBlue        = fromJust $ readHexColor "#56fca2"
warmPink              = fromJust $ readHexColor "#fb5581"
dodgerBlue            = fromJust $ readHexColor "#3e82fc"
grossGreen            = fromJust $ readHexColor "#a0bf16"
ice                   = fromJust $ readHexColor "#d6fffa"
metallicBlue          = fromJust $ readHexColor "#4f738e"
paleSalmon            = fromJust $ readHexColor "#ffb19a"
sapGreen              = fromJust $ readHexColor "#5c8b15"
algae                 = fromJust $ readHexColor "#54ac68"
blueyGrey             = fromJust $ readHexColor "#89a0b0"
greenyGrey            = fromJust $ readHexColor "#7ea07a"
highlighterGreen      = fromJust $ readHexColor "#1bfc06"
lightLightBlue        = fromJust $ readHexColor "#cafffb"
lightMint             = fromJust $ readHexColor "#b6ffbb"
rawUmber              = fromJust $ readHexColor "#a75e09"
vividBlue             = fromJust $ readHexColor "#152eff"
deepLavender          = fromJust $ readHexColor "#8d5eb7"
dullTeal              = fromJust $ readHexColor "#5f9e8f"
lightGreenishBlue     = fromJust $ readHexColor "#63f7b4"
mudGreen              = fromJust $ readHexColor "#606602"
pinky                 = fromJust $ readHexColor "#fc86aa"
redWine               = fromJust $ readHexColor "#8c0034"
shitGreen             = fromJust $ readHexColor "#758000"
tanBrown              = fromJust $ readHexColor "#ab7e4c"
darkblue              = fromJust $ readHexColor "#030764"
rosa                  = fromJust $ readHexColor "#fe86a4"
lipstick              = fromJust $ readHexColor "#d5174e"
paleMauve             = fromJust $ readHexColor "#fed0fc"
claret                = fromJust $ readHexColor "#680018"
dandelion             = fromJust $ readHexColor "#fedf08"
orangered             = fromJust $ readHexColor "#fe420f"
poopGreen             = fromJust $ readHexColor "#6f7c00"
ruby                  = fromJust $ readHexColor "#ca0147"
dark                  = fromJust $ readHexColor "#1b2431"
greenishTurquoise     = fromJust $ readHexColor "#00fbb0"
pastelRed             = fromJust $ readHexColor "#db5856"
pissYellow            = fromJust $ readHexColor "#ddd618"
brightCyan            = fromJust $ readHexColor "#41fdfe"
darkCoral             = fromJust $ readHexColor "#cf524e"
algaeGreen            = fromJust $ readHexColor "#21c36f"
darkishRed            = fromJust $ readHexColor "#a90308"
reddyBrown            = fromJust $ readHexColor "#6e1005"
blushPink             = fromJust $ readHexColor "#fe828c"
camouflageGreen       = fromJust $ readHexColor "#4b6113"
lawnGreen             = fromJust $ readHexColor "#4da409"
putty                 = fromJust $ readHexColor "#beae8a"
vibrantBlue           = fromJust $ readHexColor "#0339f8"
darkSand              = fromJust $ readHexColor "#a88f59"
purple_blue           = fromJust $ readHexColor "#5d21d0"
saffron               = fromJust $ readHexColor "#feb209"
twilight              = fromJust $ readHexColor "#4e518b"
warmBrown             = fromJust $ readHexColor "#964e02"
bluegrey              = fromJust $ readHexColor "#85a3b2"
bubbleGumPink         = fromJust $ readHexColor "#ff69af"
duckEggBlue           = fromJust $ readHexColor "#c3fbf4"
greenishCyan          = fromJust $ readHexColor "#2afeb7"
petrol                = fromJust $ readHexColor "#005f6a"
royal                 = fromJust $ readHexColor "#0c1793"
butter                = fromJust $ readHexColor "#ffff81"
dustyOrange           = fromJust $ readHexColor "#f0833a"
offYellow             = fromJust $ readHexColor "#f1f33f"
paleOliveGreen        = fromJust $ readHexColor "#b1d27b"
orangish              = fromJust $ readHexColor "#fc824a"
leaf                  = fromJust $ readHexColor "#71aa34"
lightBlueGrey         = fromJust $ readHexColor "#b7c9e2"
driedBlood            = fromJust $ readHexColor "#4b0101"
lightishPurple        = fromJust $ readHexColor "#a552e6"
rustyRed              = fromJust $ readHexColor "#af2f0d"
lavenderBlue          = fromJust $ readHexColor "#8b88f8"
lightGrassGreen       = fromJust $ readHexColor "#9af764"
lightMintGreen        = fromJust $ readHexColor "#a6fbb2"
sunflower             = fromJust $ readHexColor "#ffc512"
velvet                = fromJust $ readHexColor "#750851"
brickOrange           = fromJust $ readHexColor "#c14a09"
lightishRed           = fromJust $ readHexColor "#fe2f4a"
pureBlue              = fromJust $ readHexColor "#0203e2"
twilightBlue          = fromJust $ readHexColor "#0a437a"
violetRed             = fromJust $ readHexColor "#a50055"
yellowyBrown          = fromJust $ readHexColor "#ae8b0c"
carnation             = fromJust $ readHexColor "#fd798f"
muddyYellow           = fromJust $ readHexColor "#bfac05"
darkSeafoamGreen      = fromJust $ readHexColor "#3eaf76"
deepRose              = fromJust $ readHexColor "#c74767"
dustyRed              = fromJust $ readHexColor "#b9484e"
grey_blue             = fromJust $ readHexColor "#647d8e"
lemonLime             = fromJust $ readHexColor "#bffe28"
purple_pink           = fromJust $ readHexColor "#d725de"
brownYellow           = fromJust $ readHexColor "#b29705"
purpleBrown           = fromJust $ readHexColor "#673a3f"
wisteria              = fromJust $ readHexColor "#a87dc2"
bananaYellow          = fromJust $ readHexColor "#fafe4b"
lipstickRed           = fromJust $ readHexColor "#c0022f"
waterBlue             = fromJust $ readHexColor "#0e87cc"
brownGrey             = fromJust $ readHexColor "#8d8468"
vibrantPurple         = fromJust $ readHexColor "#ad03de"
babyGreen             = fromJust $ readHexColor "#8cff9e"
barfGreen             = fromJust $ readHexColor "#94ac02"
eggshellBlue          = fromJust $ readHexColor "#c4fff7"
sandyYellow           = fromJust $ readHexColor "#fdee73"
coolGreen             = fromJust $ readHexColor "#33b864"
pale                  = fromJust $ readHexColor "#fff9d0"
blue_grey             = fromJust $ readHexColor "#758da3"
hotMagenta            = fromJust $ readHexColor "#f504c9"
greyblue              = fromJust $ readHexColor "#77a1b5"
purpley               = fromJust $ readHexColor "#8756e4"
babyShitGreen         = fromJust $ readHexColor "#889717"
brownishPink          = fromJust $ readHexColor "#c27e79"
darkAquamarine        = fromJust $ readHexColor "#017371"
diarrhea              = fromJust $ readHexColor "#9f8303"
lightMustard          = fromJust $ readHexColor "#f7d560"
paleSkyBlue           = fromJust $ readHexColor "#bdf6fe"
turtleGreen           = fromJust $ readHexColor "#75b84f"
brightOlive           = fromJust $ readHexColor "#9cbb04"
darkGreyBlue          = fromJust $ readHexColor "#29465b"
greenyBrown           = fromJust $ readHexColor "#696006"
lemonGreen            = fromJust $ readHexColor "#adf802"
lightPeriwinkle       = fromJust $ readHexColor "#c1c6fc"
seaweedGreen          = fromJust $ readHexColor "#35ad6b"
sunshineYellow        = fromJust $ readHexColor "#fffd37"
uglyPurple            = fromJust $ readHexColor "#a442a0"
mediumPink            = fromJust $ readHexColor "#f36196"
pukeBrown             = fromJust $ readHexColor "#947706"
veryLightPink         = fromJust $ readHexColor "#fff4f2"
viridian              = fromJust $ readHexColor "#1e9167"
bile                  = fromJust $ readHexColor "#b5c306"
fadedYellow           = fromJust $ readHexColor "#feff7f"
veryPaleGreen         = fromJust $ readHexColor "#cffdbc"
vibrantGreen          = fromJust $ readHexColor "#0add08"
brightLime            = fromJust $ readHexColor "#87fd05"
spearmint             = fromJust $ readHexColor "#1ef876"
lightAquamarine       = fromJust $ readHexColor "#7bfdc7"
lightSage             = fromJust $ readHexColor "#bcecac"
yellowgreen           = fromJust $ readHexColor "#bbf90f"
babyPoo               = fromJust $ readHexColor "#ab9004"
darkSeafoam           = fromJust $ readHexColor "#1fb57a"
deepTeal              = fromJust $ readHexColor "#00555a"
heather               = fromJust $ readHexColor "#a484ac"
rustOrange            = fromJust $ readHexColor "#c45508"
dirtyBlue             = fromJust $ readHexColor "#3f829d"
fernGreen             = fromJust $ readHexColor "#548d44"
brightLilac           = fromJust $ readHexColor "#c95efb"
weirdGreen            = fromJust $ readHexColor "#3ae57f"
peacockBlue           = fromJust $ readHexColor "#016795"
avocadoGreen          = fromJust $ readHexColor "#87a922"
fadedOrange           = fromJust $ readHexColor "#f0944d"
grapePurple           = fromJust $ readHexColor "#5d1451"
hotGreen              = fromJust $ readHexColor "#25ff29"
limeYellow            = fromJust $ readHexColor "#d0fe1d"
mango                 = fromJust $ readHexColor "#ffa62b"
shamrock              = fromJust $ readHexColor "#01b44c"
bubblegum             = fromJust $ readHexColor "#ff6cb5"
purplishBrown         = fromJust $ readHexColor "#6b4247"
vomitYellow           = fromJust $ readHexColor "#c7c10c"
paleCyan              = fromJust $ readHexColor "#b7fffa"
keyLime               = fromJust $ readHexColor "#aeff6e"
tomatoRed             = fromJust $ readHexColor "#ec2d01"
lightgreen            = fromJust $ readHexColor "#76ff7b"
merlot                = fromJust $ readHexColor "#730039"
nightBlue             = fromJust $ readHexColor "#040348"
purpleishPink         = fromJust $ readHexColor "#df4ec8"
apple                 = fromJust $ readHexColor "#6ecb3c"
babyPoopGreen         = fromJust $ readHexColor "#8f9805"
greenApple            = fromJust $ readHexColor "#5edc1f"
heliotrope            = fromJust $ readHexColor "#d94ff5"
yellow_green          = fromJust $ readHexColor "#c8fd3d"
almostBlack           = fromJust $ readHexColor "#070d0d"
coolBlue              = fromJust $ readHexColor "#4984b8"
leafyGreen            = fromJust $ readHexColor "#51b73b"
mustardBrown          = fromJust $ readHexColor "#ac7e04"
dusk                  = fromJust $ readHexColor "#4e5481"
dullBrown             = fromJust $ readHexColor "#876e4b"
frogGreen             = fromJust $ readHexColor "#58bc08"
vividGreen            = fromJust $ readHexColor "#2fef10"
brightLightGreen      = fromJust $ readHexColor "#2dfe54"
fluroGreen            = fromJust $ readHexColor "#0aff02"
kiwi                  = fromJust $ readHexColor "#9cef43"
seaweed               = fromJust $ readHexColor "#18d17b"
navyGreen             = fromJust $ readHexColor "#35530a"
ultramarineBlue       = fromJust $ readHexColor "#1805db"
iris                  = fromJust $ readHexColor "#6258c4"
pastelOrange          = fromJust $ readHexColor "#ff964f"
yellowishOrange       = fromJust $ readHexColor "#ffab0f"
perrywinkle           = fromJust $ readHexColor "#8f8ce7"
tealish               = fromJust $ readHexColor "#24bca8"
darkPlum              = fromJust $ readHexColor "#3f012c"
pear                  = fromJust $ readHexColor "#cbf85f"
pinkishOrange         = fromJust $ readHexColor "#ff724c"
midnightPurple        = fromJust $ readHexColor "#280137"
lightUrple            = fromJust $ readHexColor "#b36ff6"
darkMint              = fromJust $ readHexColor "#48c072"
greenishTan           = fromJust $ readHexColor "#bccb7a"
lightBurgundy         = fromJust $ readHexColor "#a8415b"
turquoiseBlue         = fromJust $ readHexColor "#06b1c4"
uglyPink              = fromJust $ readHexColor "#cd7584"
sandy                 = fromJust $ readHexColor "#f1da7a"
electricPink          = fromJust $ readHexColor "#ff0490"
mutedPurple           = fromJust $ readHexColor "#805b87"
midGreen              = fromJust $ readHexColor "#50a747"
greyish               = fromJust $ readHexColor "#a8a495"
neonYellow            = fromJust $ readHexColor "#cfff04"
banana                = fromJust $ readHexColor "#ffff7e"
carnationPink         = fromJust $ readHexColor "#ff7fa7"
tomato                = fromJust $ readHexColor "#ef4026"
sea                   = fromJust $ readHexColor "#3c9992"
muddyBrown            = fromJust $ readHexColor "#886806"
turquoiseGreen        = fromJust $ readHexColor "#04f489"
buff                  = fromJust $ readHexColor "#fef69e"
fawn                  = fromJust $ readHexColor "#cfaf7b"
mutedBlue             = fromJust $ readHexColor "#3b719f"
paleRose              = fromJust $ readHexColor "#fdc1c5"
darkMintGreen         = fromJust $ readHexColor "#20c073"
amethyst              = fromJust $ readHexColor "#9b5fc0"
blue_green            = fromJust $ readHexColor "#0f9b8e"
chestnut              = fromJust $ readHexColor "#742802"
sickGreen             = fromJust $ readHexColor "#9db92c"
pea                   = fromJust $ readHexColor "#a4bf20"
rustyOrange           = fromJust $ readHexColor "#cd5909"
stone                 = fromJust $ readHexColor "#ada587"
roseRed               = fromJust $ readHexColor "#be013c"
paleAqua              = fromJust $ readHexColor "#b8ffeb"
deepOrange            = fromJust $ readHexColor "#dc4d01"
earth                 = fromJust $ readHexColor "#a2653e"
mossyGreen            = fromJust $ readHexColor "#638b27"
grassyGreen           = fromJust $ readHexColor "#419c03"
paleLimeGreen         = fromJust $ readHexColor "#b1ff65"
lightGreyBlue         = fromJust $ readHexColor "#9dbcd4"
paleGrey              = fromJust $ readHexColor "#fdfdfe"
asparagus             = fromJust $ readHexColor "#77ab56"
blueberry             = fromJust $ readHexColor "#464196"
purpleRed             = fromJust $ readHexColor "#990147"
paleLime              = fromJust $ readHexColor "#befd73"
greenishTeal          = fromJust $ readHexColor "#32bf84"
caramel               = fromJust $ readHexColor "#af6f09"
deepMagenta           = fromJust $ readHexColor "#a0025c"
lightPeach            = fromJust $ readHexColor "#ffd8b1"
milkChocolate         = fromJust $ readHexColor "#7f4e1e"
ocher                 = fromJust $ readHexColor "#bf9b0c"
offGreen              = fromJust $ readHexColor "#6ba353"
purplyPink            = fromJust $ readHexColor "#f075e6"
lightblue             = fromJust $ readHexColor "#7bc8f6"
duskyBlue             = fromJust $ readHexColor "#475f94"
golden                = fromJust $ readHexColor "#f5bf03"
lightBeige            = fromJust $ readHexColor "#fffeb6"
butterYellow          = fromJust $ readHexColor "#fffd74"
duskyPurple           = fromJust $ readHexColor "#895b7b"
frenchBlue            = fromJust $ readHexColor "#436bad"
uglyYellow            = fromJust $ readHexColor "#d0c101"
greenyYellow          = fromJust $ readHexColor "#c6f808"
orangishRed           = fromJust $ readHexColor "#f43605"
shamrockGreen         = fromJust $ readHexColor "#02c14d"
orangishBrown         = fromJust $ readHexColor "#b25f03"
treeGreen             = fromJust $ readHexColor "#2a7e19"
deepViolet            = fromJust $ readHexColor "#490648"
gunmetal              = fromJust $ readHexColor "#536267"
blue_purple           = fromJust $ readHexColor "#5a06ef"
cherry                = fromJust $ readHexColor "#cf0234"
sandyBrown            = fromJust $ readHexColor "#c4a661"
warmGrey              = fromJust $ readHexColor "#978a84"
darkIndigo            = fromJust $ readHexColor "#1f0954"
midnight              = fromJust $ readHexColor "#03012d"
blueyGreen            = fromJust $ readHexColor "#2bb179"
greyPink              = fromJust $ readHexColor "#c3909b"
softPurple            = fromJust $ readHexColor "#a66fb5"
blood                 = fromJust $ readHexColor "#770001"
brownRed              = fromJust $ readHexColor "#922b05"
mediumGrey            = fromJust $ readHexColor "#7d7f7c"
berry                 = fromJust $ readHexColor "#990f4b"
poo                   = fromJust $ readHexColor "#8f7303"
purpleyPink           = fromJust $ readHexColor "#c83cb9"
lightSalmon           = fromJust $ readHexColor "#fea993"
snot                  = fromJust $ readHexColor "#acbb0d"
easterPurple          = fromJust $ readHexColor "#c071fe"
lightYellowGreen      = fromJust $ readHexColor "#ccfd7f"
darkNavyBlue          = fromJust $ readHexColor "#00022e"
drab                  = fromJust $ readHexColor "#828344"
lightRose             = fromJust $ readHexColor "#ffc5cb"
rouge                 = fromJust $ readHexColor "#ab1239"
purplishRed           = fromJust $ readHexColor "#b0054b"
slimeGreen            = fromJust $ readHexColor "#99cc04"
babyPoop              = fromJust $ readHexColor "#937c00"
irishGreen            = fromJust $ readHexColor "#019529"
pink_purple           = fromJust $ readHexColor "#ef1de7"
darkNavy              = fromJust $ readHexColor "#000435"
greenyBlue            = fromJust $ readHexColor "#42b395"
lightPlum             = fromJust $ readHexColor "#9d5783"
pinkishGrey           = fromJust $ readHexColor "#c8aca9"
dirtyOrange           = fromJust $ readHexColor "#c87606"
rustRed               = fromJust $ readHexColor "#aa2704"
paleLilac             = fromJust $ readHexColor "#e4cbff"
orangeyRed            = fromJust $ readHexColor "#fa4224"
primaryBlue           = fromJust $ readHexColor "#0804f9"
kermitGreen           = fromJust $ readHexColor "#5cb200"
brownishPurple        = fromJust $ readHexColor "#76424e"
murkyGreen            = fromJust $ readHexColor "#6c7a0e"
wheat                 = fromJust $ readHexColor "#fbdd7e"
veryDarkPurple        = fromJust $ readHexColor "#2a0134"
bottleGreen           = fromJust $ readHexColor "#044a05"
watermelon            = fromJust $ readHexColor "#fd4659"
deepSkyBlue           = fromJust $ readHexColor "#0d75f8"
fireEngineRed         = fromJust $ readHexColor "#fe0002"
yellowOchre           = fromJust $ readHexColor "#cb9d06"
pumpkinOrange         = fromJust $ readHexColor "#fb7d07"
paleOlive             = fromJust $ readHexColor "#b9cc81"
lightLilac            = fromJust $ readHexColor "#edc8ff"
lightishGreen         = fromJust $ readHexColor "#61e160"
carolinaBlue          = fromJust $ readHexColor "#8ab8fe"
mulberry              = fromJust $ readHexColor "#920a4e"
shockingPink          = fromJust $ readHexColor "#fe02a2"
auburn                = fromJust $ readHexColor "#9a3001"
brightLimeGreen       = fromJust $ readHexColor "#65fe08"
celadon               = fromJust $ readHexColor "#befdb7"
pinkishBrown          = fromJust $ readHexColor "#b17261"
pooBrown              = fromJust $ readHexColor "#885f01"
brightSkyBlue         = fromJust $ readHexColor "#02ccfe"
celery                = fromJust $ readHexColor "#c1fd95"
dirtBrown             = fromJust $ readHexColor "#836539"
strawberry            = fromJust $ readHexColor "#fb2943"
darkLime              = fromJust $ readHexColor "#84b701"
copper                = fromJust $ readHexColor "#b66325"
mediumBrown           = fromJust $ readHexColor "#7f5112"
mutedGreen            = fromJust $ readHexColor "#5fa052"
robin'sEgg            = fromJust $ readHexColor "#6dedfd"
brightAqua            = fromJust $ readHexColor "#0bf9ea"
brightLavender        = fromJust $ readHexColor "#c760ff"
ivory                 = fromJust $ readHexColor "#ffffcb"
veryLightPurple       = fromJust $ readHexColor "#f6cefc"
lightNavy             = fromJust $ readHexColor "#155084"
pinkRed               = fromJust $ readHexColor "#f5054f"
oliveBrown            = fromJust $ readHexColor "#645403"
poopBrown             = fromJust $ readHexColor "#7a5901"
mustardGreen          = fromJust $ readHexColor "#a8b504"
oceanGreen            = fromJust $ readHexColor "#3d9973"
veryDarkBlue          = fromJust $ readHexColor "#000133"
dustyGreen            = fromJust $ readHexColor "#76a973"
lightNavyBlue         = fromJust $ readHexColor "#2e5a88"
mintyGreen            = fromJust $ readHexColor "#0bf77d"
adobe                 = fromJust $ readHexColor "#bd6c48"
barney                = fromJust $ readHexColor "#ac1db8"
jadeGreen             = fromJust $ readHexColor "#2baf6a"
brightLightBlue       = fromJust $ readHexColor "#26f7fd"
lightLime             = fromJust $ readHexColor "#aefd6c"
darkKhaki             = fromJust $ readHexColor "#9b8f55"
orangeYellow          = fromJust $ readHexColor "#ffad01"
ocre                  = fromJust $ readHexColor "#c69c04"
maize                 = fromJust $ readHexColor "#f4d054"
fadedPink             = fromJust $ readHexColor "#de9dac"
britishRacingGreen    = fromJust $ readHexColor "#05480d"
sandstone             = fromJust $ readHexColor "#c9ae74"
mudBrown              = fromJust $ readHexColor "#60460f"
lightSeaGreen         = fromJust $ readHexColor "#98f6b0"
robinEggBlue          = fromJust $ readHexColor "#8af1fe"
aquaMarine            = fromJust $ readHexColor "#2ee8bb"
darkSeaGreen          = fromJust $ readHexColor "#11875d"
softPink              = fromJust $ readHexColor "#fdb0c0"
orangeyBrown          = fromJust $ readHexColor "#b16002"
cherryRed             = fromJust $ readHexColor "#f7022a"
burntYellow           = fromJust $ readHexColor "#d5ab09"
brownishGrey          = fromJust $ readHexColor "#86775f"
camel                 = fromJust $ readHexColor "#c69f59"
purplishGrey          = fromJust $ readHexColor "#7a687f"
marine                = fromJust $ readHexColor "#042e60"
greyishPink           = fromJust $ readHexColor "#c88d94"
paleTurquoise         = fromJust $ readHexColor "#a5fbd5"
pastelYellow          = fromJust $ readHexColor "#fffe71"
blueyPurple           = fromJust $ readHexColor "#6241c7"
canaryYellow          = fromJust $ readHexColor "#fffe40"
fadedRed              = fromJust $ readHexColor "#d3494e"
sepia                 = fromJust $ readHexColor "#985e2b"
coffee                = fromJust $ readHexColor "#a6814c"
brightMagenta         = fromJust $ readHexColor "#ff08e8"
mocha                 = fromJust $ readHexColor "#9d7651"
ecru                  = fromJust $ readHexColor "#feffca"
purpleish             = fromJust $ readHexColor "#98568d"
cranberry             = fromJust $ readHexColor "#9e003a"
darkishGreen          = fromJust $ readHexColor "#287c37"
brownOrange           = fromJust $ readHexColor "#b96902"
duskyRose             = fromJust $ readHexColor "#ba6873"
melon                 = fromJust $ readHexColor "#ff7855"
sicklyGreen           = fromJust $ readHexColor "#94b21c"
silver                = fromJust $ readHexColor "#c5c9c7"
purplyBlue            = fromJust $ readHexColor "#661aee"
purpleishBlue         = fromJust $ readHexColor "#6140ef"
hospitalGreen         = fromJust $ readHexColor "#9be5aa"
shitBrown             = fromJust $ readHexColor "#7b5804"
midBlue               = fromJust $ readHexColor "#276ab3"
amber                 = fromJust $ readHexColor "#feb308"
easterGreen           = fromJust $ readHexColor "#8cfd7e"
softBlue              = fromJust $ readHexColor "#6488ea"
ceruleanBlue          = fromJust $ readHexColor "#056eee"
goldenBrown           = fromJust $ readHexColor "#b27a01"
brightTurquoise       = fromJust $ readHexColor "#0ffef9"
redPink               = fromJust $ readHexColor "#fa2a55"
redPurple             = fromJust $ readHexColor "#820747"
greyishBrown          = fromJust $ readHexColor "#7a6a4f"
vermillion            = fromJust $ readHexColor "#f4320c"
russet                = fromJust $ readHexColor "#a13905"
steelGrey             = fromJust $ readHexColor "#6f828a"
lighterPurple         = fromJust $ readHexColor "#a55af4"
brightViolet          = fromJust $ readHexColor "#ad0afd"
prussianBlue          = fromJust $ readHexColor "#004577"
slateGreen            = fromJust $ readHexColor "#658d6d"
dirtyPink             = fromJust $ readHexColor "#ca7b80"
darkBlueGreen         = fromJust $ readHexColor "#005249"
pine                  = fromJust $ readHexColor "#2b5d34"
yellowyGreen          = fromJust $ readHexColor "#bff128"
darkGold              = fromJust $ readHexColor "#b59410"
bluish                = fromJust $ readHexColor "#2976bb"
darkishBlue           = fromJust $ readHexColor "#014182"
dullRed               = fromJust $ readHexColor "#bb3f3f"
pinkyRed              = fromJust $ readHexColor "#fc2647"
bronze                = fromJust $ readHexColor "#a87900"
paleTeal              = fromJust $ readHexColor "#82cbb2"
militaryGreen         = fromJust $ readHexColor "#667c3e"
barbiePink            = fromJust $ readHexColor "#fe46a5"
bubblegumPink         = fromJust $ readHexColor "#fe83cc"
peaSoupGreen          = fromJust $ readHexColor "#94a617"
darkMustard           = fromJust $ readHexColor "#a88905"
shit                  = fromJust $ readHexColor "#7f5f00"
mediumPurple          = fromJust $ readHexColor "#9e43a2"
veryDarkGreen         = fromJust $ readHexColor "#062e03"
dirt                  = fromJust $ readHexColor "#8a6e45"
duskyPink             = fromJust $ readHexColor "#cc7a8b"
redViolet             = fromJust $ readHexColor "#9e0168"
lemonYellow           = fromJust $ readHexColor "#fdff38"
pistachio             = fromJust $ readHexColor "#c0fa8b"
dullYellow            = fromJust $ readHexColor "#eedc5b"
darkLimeGreen         = fromJust $ readHexColor "#7ebd01"
denimBlue             = fromJust $ readHexColor "#3b5b92"
tealBlue              = fromJust $ readHexColor "#01889f"
lightishBlue          = fromJust $ readHexColor "#3d7afd"
purpleyBlue           = fromJust $ readHexColor "#5f34e7"
lightIndigo           = fromJust $ readHexColor "#6d5acf"
swampGreen            = fromJust $ readHexColor "#748500"
brownGreen            = fromJust $ readHexColor "#706c11"
darkMaroon            = fromJust $ readHexColor "#3c0008"
hotPurple             = fromJust $ readHexColor "#cb00f5"
darkForestGreen       = fromJust $ readHexColor "#002d04"
fadedBlue             = fromJust $ readHexColor "#658cbb"
drabGreen             = fromJust $ readHexColor "#749551"
lightLimeGreen        = fromJust $ readHexColor "#b9ff66"
snotGreen             = fromJust $ readHexColor "#9dc100"
yellowish             = fromJust $ readHexColor "#faee66"
lightBlueGreen        = fromJust $ readHexColor "#7efbb3"
bordeaux              = fromJust $ readHexColor "#7b002c"
lightMauve            = fromJust $ readHexColor "#c292a1"
ocean                 = fromJust $ readHexColor "#017b92"
marigold              = fromJust $ readHexColor "#fcc006"
muddyGreen            = fromJust $ readHexColor "#657432"
dullOrange            = fromJust $ readHexColor "#d8863b"
steel                 = fromJust $ readHexColor "#738595"
electricPurple        = fromJust $ readHexColor "#aa23ff"
fluorescentGreen      = fromJust $ readHexColor "#08ff08"
yellowishBrown        = fromJust $ readHexColor "#9b7a01"
blush                 = fromJust $ readHexColor "#f29e8e"
softGreen             = fromJust $ readHexColor "#6fc276"
brightOrange          = fromJust $ readHexColor "#ff5b00"
lemon                 = fromJust $ readHexColor "#fdff52"
purpleGrey            = fromJust $ readHexColor "#866f85"
acidGreen             = fromJust $ readHexColor "#8ffe09"
paleLavender          = fromJust $ readHexColor "#eecffe"
violetBlue            = fromJust $ readHexColor "#510ac9"
lightForestGreen      = fromJust $ readHexColor "#4f9153"
burntRed              = fromJust $ readHexColor "#9f2305"
khakiGreen            = fromJust $ readHexColor "#728639"
cerise                = fromJust $ readHexColor "#de0c62"
fadedPurple           = fromJust $ readHexColor "#916e99"
apricot               = fromJust $ readHexColor "#ffb16d"
darkOliveGreen        = fromJust $ readHexColor "#3c4d03"
greyBrown             = fromJust $ readHexColor "#7f7053"
greenGrey             = fromJust $ readHexColor "#77926f"
trueBlue              = fromJust $ readHexColor "#010fcc"
paleViolet            = fromJust $ readHexColor "#ceaefa"
periwinkleBlue        = fromJust $ readHexColor "#8f99fb"
lightSkyBlue          = fromJust $ readHexColor "#c6fcff"
blurple               = fromJust $ readHexColor "#5539cc"
greenBrown            = fromJust $ readHexColor "#544e03"
bluegreen             = fromJust $ readHexColor "#017a79"
brightTeal            = fromJust $ readHexColor "#01f9c6"
brownishYellow        = fromJust $ readHexColor "#c9b003"
peaSoup               = fromJust $ readHexColor "#929901"
forest                = fromJust $ readHexColor "#0b5509"
barneyPurple          = fromJust $ readHexColor "#a00498"
ultramarine           = fromJust $ readHexColor "#2000b1"
purplish              = fromJust $ readHexColor "#94568c"
pukeYellow            = fromJust $ readHexColor "#c2be0e"
bluishGrey            = fromJust $ readHexColor "#748b97"
darkPeriwinkle        = fromJust $ readHexColor "#665fd1"
darkLilac             = fromJust $ readHexColor "#9c6da5"
reddish               = fromJust $ readHexColor "#c44240"
lightMaroon           = fromJust $ readHexColor "#a24857"
dustyPurple           = fromJust $ readHexColor "#825f87"
terraCotta            = fromJust $ readHexColor "#c9643b"
avocado               = fromJust $ readHexColor "#90b134"
marineBlue            = fromJust $ readHexColor "#01386a"
tealGreen             = fromJust $ readHexColor "#25a36f"
slateGrey             = fromJust $ readHexColor "#59656d"
lighterGreen          = fromJust $ readHexColor "#75fd63"
electricGreen         = fromJust $ readHexColor "#21fc0d"
dustyBlue             = fromJust $ readHexColor "#5a86ad"
goldenYellow          = fromJust $ readHexColor "#fec615"
brightYellow          = fromJust $ readHexColor "#fffd01"
lightLavender         = fromJust $ readHexColor "#dfc5fe"
umber                 = fromJust $ readHexColor "#b26400"
poop                  = fromJust $ readHexColor "#7f5e00"
darkPeach             = fromJust $ readHexColor "#de7e5d"
jungleGreen           = fromJust $ readHexColor "#048243"
eggshell              = fromJust $ readHexColor "#ffffd4"
denim                 = fromJust $ readHexColor "#3b638c"
yellowBrown           = fromJust $ readHexColor "#b79400"
dullPurple            = fromJust $ readHexColor "#84597e"
chocolateBrown        = fromJust $ readHexColor "#411900"
wineRed               = fromJust $ readHexColor "#7b0323"
neonBlue              = fromJust $ readHexColor "#04d9ff"
dirtyGreen            = fromJust $ readHexColor "#667e2c"
lightTan              = fromJust $ readHexColor "#fbeeac"
iceBlue               = fromJust $ readHexColor "#d7fffe"
cadetBlue             = fromJust $ readHexColor "#4e7496"
darkMauve             = fromJust $ readHexColor "#874c62"
veryLightBlue         = fromJust $ readHexColor "#d5ffff"
greyPurple            = fromJust $ readHexColor "#826d8c"
pastelPink            = fromJust $ readHexColor "#ffbacd"
veryLightGreen        = fromJust $ readHexColor "#d1ffbd"
darkSkyBlue           = fromJust $ readHexColor "#448ee4"
evergreen             = fromJust $ readHexColor "#05472a"
dullPink              = fromJust $ readHexColor "#d5869d"
aubergine             = fromJust $ readHexColor "#3d0734"
mahogany              = fromJust $ readHexColor "#4a0100"
reddishOrange         = fromJust $ readHexColor "#f8481c"
deepGreen             = fromJust $ readHexColor "#02590f"
vomitGreen            = fromJust $ readHexColor "#89a203"
purplePink            = fromJust $ readHexColor "#e03fd8"
dustyPink             = fromJust $ readHexColor "#d58a94"
fadedGreen            = fromJust $ readHexColor "#7bb274"
camoGreen             = fromJust $ readHexColor "#526525"
pinkyPurple           = fromJust $ readHexColor "#c94cbe"
pinkPurple            = fromJust $ readHexColor "#db4bda"
brownishRed           = fromJust $ readHexColor "#9e3623"
darkRose              = fromJust $ readHexColor "#b5485d"
mud                   = fromJust $ readHexColor "#735c12"
brownish              = fromJust $ readHexColor "#9c6d57"
emeraldGreen          = fromJust $ readHexColor "#028f1e"
paleBrown             = fromJust $ readHexColor "#b1916e"
dullBlue              = fromJust $ readHexColor "#49759c"
burntUmber            = fromJust $ readHexColor "#a0450e"
mediumGreen           = fromJust $ readHexColor "#39ad48"
clay                  = fromJust $ readHexColor "#b66a50"
lightAqua             = fromJust $ readHexColor "#8cffdb"
lightOliveGreen       = fromJust $ readHexColor "#a4be5c"
brownishOrange        = fromJust $ readHexColor "#cb7723"
darkAqua              = fromJust $ readHexColor "#05696b"
purplishPink          = fromJust $ readHexColor "#ce5dae"
darkSalmon            = fromJust $ readHexColor "#c85a53"
greenishGrey          = fromJust $ readHexColor "#96ae8d"
jade                  = fromJust $ readHexColor "#1fa774"
uglyGreen             = fromJust $ readHexColor "#7a9703"
darkBeige             = fromJust $ readHexColor "#ac9362"
emerald               = fromJust $ readHexColor "#01a049"
paleRed               = fromJust $ readHexColor "#d9544d"
lightMagenta          = fromJust $ readHexColor "#fa5ff7"
sky                   = fromJust $ readHexColor "#82cafc"
lightCyan             = fromJust $ readHexColor "#acfffc"
yellowOrange          = fromJust $ readHexColor "#fcb001"
reddishPurple         = fromJust $ readHexColor "#910951"
reddishPink           = fromJust $ readHexColor "#fe2c54"
orchid                = fromJust $ readHexColor "#c875c4"
dirtyYellow           = fromJust $ readHexColor "#cdc50a"
orangeRed             = fromJust $ readHexColor "#fd411e"
deepRed               = fromJust $ readHexColor "#9a0200"
orangeBrown           = fromJust $ readHexColor "#be6400"
cobaltBlue            = fromJust $ readHexColor "#030aa7"
neonPink              = fromJust $ readHexColor "#fe019a"
rosePink              = fromJust $ readHexColor "#f7879a"
greyishPurple         = fromJust $ readHexColor "#887191"
raspberry             = fromJust $ readHexColor "#b00149"
aquaGreen             = fromJust $ readHexColor "#12e193"
salmonPink            = fromJust $ readHexColor "#fe7b7c"
tangerine             = fromJust $ readHexColor "#ff9408"
brownishGreen         = fromJust $ readHexColor "#6a6e09"
redBrown              = fromJust $ readHexColor "#8b2e16"
greenishBrown         = fromJust $ readHexColor "#696112"
pumpkin               = fromJust $ readHexColor "#e17701"
pineGreen             = fromJust $ readHexColor "#0a481e"
charcoal              = fromJust $ readHexColor "#343837"
babyPink              = fromJust $ readHexColor "#ffb7ce"
cornflower            = fromJust $ readHexColor "#6a79f7"
blueViolet            = fromJust $ readHexColor "#5d06e9"
chocolate             = fromJust $ readHexColor "#3d1c02"
greyishGreen          = fromJust $ readHexColor "#82a67d"
scarlet               = fromJust $ readHexColor "#be0119"
greenYellow           = fromJust $ readHexColor "#c9ff27"
darkOlive             = fromJust $ readHexColor "#373e02"
sienna                = fromJust $ readHexColor "#a9561e"
pastelPurple          = fromJust $ readHexColor "#caa0ff"
terracotta            = fromJust $ readHexColor "#ca6641"
aquaBlue              = fromJust $ readHexColor "#02d8e9"
sageGreen             = fromJust $ readHexColor "#88b378"
bloodRed              = fromJust $ readHexColor "#980002"
deepPink              = fromJust $ readHexColor "#cb0162"
grass                 = fromJust $ readHexColor "#5cac2d"
moss                  = fromJust $ readHexColor "#769958"
pastelBlue            = fromJust $ readHexColor "#a2bffe"
bluishGreen           = fromJust $ readHexColor "#10a674"
greenBlue             = fromJust $ readHexColor "#06b48b"
darkTan               = fromJust $ readHexColor "#af884a"
greenishBlue          = fromJust $ readHexColor "#0b8b87"
paleOrange            = fromJust $ readHexColor "#ffa756"
vomit                 = fromJust $ readHexColor "#a2a415"
forrestGreen          = fromJust $ readHexColor "#154406"
darkLavender          = fromJust $ readHexColor "#856798"
darkViolet            = fromJust $ readHexColor "#34013f"
purpleBlue            = fromJust $ readHexColor "#632de9"
darkCyan              = fromJust $ readHexColor "#0a888a"
oliveDrab             = fromJust $ readHexColor "#6f7632"
pinkish               = fromJust $ readHexColor "#d46a7e"
cobalt                = fromJust $ readHexColor "#1e488f"
neonPurple            = fromJust $ readHexColor "#bc13fe"
lightTurquoise        = fromJust $ readHexColor "#7ef4cc"
appleGreen            = fromJust $ readHexColor "#76cd26"
dullGreen             = fromJust $ readHexColor "#74a662"
wine                  = fromJust $ readHexColor "#80013f"
powderBlue            = fromJust $ readHexColor "#b1d1fc"
offWhite              = fromJust $ readHexColor "#ffffe4"
electricBlue          = fromJust $ readHexColor "#0652ff"
darkTurquoise         = fromJust $ readHexColor "#045c5a"
bluePurple            = fromJust $ readHexColor "#5729ce"
azure                 = fromJust $ readHexColor "#069af3"
brightRed             = fromJust $ readHexColor "#ff000d"
pinkishRed            = fromJust $ readHexColor "#f10c45"
cornflowerBlue        = fromJust $ readHexColor "#5170d7"
lightOlive            = fromJust $ readHexColor "#acbf69"
grape                 = fromJust $ readHexColor "#6c3461"
greyishBlue           = fromJust $ readHexColor "#5e819d"
purplishBlue          = fromJust $ readHexColor "#601ef9"
yellowishGreen        = fromJust $ readHexColor "#b0dd16"
greenishYellow        = fromJust $ readHexColor "#cdfd02"
mediumBlue            = fromJust $ readHexColor "#2c6fbb"
dustyRose             = fromJust $ readHexColor "#c0737a"
lightViolet           = fromJust $ readHexColor "#d6b4fc"
midnightBlue          = fromJust $ readHexColor "#020035"
bluishPurple          = fromJust $ readHexColor "#703be7"
redOrange             = fromJust $ readHexColor "#fd3c06"
darkMagenta           = fromJust $ readHexColor "#960056"
greenish              = fromJust $ readHexColor "#40a368"
oceanBlue             = fromJust $ readHexColor "#03719c"
coral                 = fromJust $ readHexColor "#fc5a50"
cream                 = fromJust $ readHexColor "#ffffc2"
reddishBrown          = fromJust $ readHexColor "#7f2b0a"
burntSienna           = fromJust $ readHexColor "#b04e0f"
brick                 = fromJust $ readHexColor "#a03623"
sage                  = fromJust $ readHexColor "#87ae73"
greyGreen             = fromJust $ readHexColor "#789b73"
white                 = fromJust $ readHexColor "#ffffff"
robin'sEggBlue        = fromJust $ readHexColor "#98eff9"
mossGreen             = fromJust $ readHexColor "#658b38"
steelBlue             = fromJust $ readHexColor "#5a7d9a"
eggplant              = fromJust $ readHexColor "#380835"
lightYellow           = fromJust $ readHexColor "#fffe7a"
leafGreen             = fromJust $ readHexColor "#5ca904"
lightGrey             = fromJust $ readHexColor "#d8dcd6"
puke                  = fromJust $ readHexColor "#a5a502"
pinkishPurple         = fromJust $ readHexColor "#d648d7"
seaBlue               = fromJust $ readHexColor "#047495"
palePurple            = fromJust $ readHexColor "#b790d4"
slateBlue             = fromJust $ readHexColor "#5b7c99"
blueGrey              = fromJust $ readHexColor "#607c8e"
hunterGreen           = fromJust $ readHexColor "#0b4008"
fuchsia               = fromJust $ readHexColor "#ed0dd9"
crimson               = fromJust $ readHexColor "#8c000f"
paleYellow            = fromJust $ readHexColor "#ffff84"
ochre                 = fromJust $ readHexColor "#bf9005"
mustardYellow         = fromJust $ readHexColor "#d2bd0a"
lightRed              = fromJust $ readHexColor "#ff474c"
cerulean              = fromJust $ readHexColor "#0485d1"
palePink              = fromJust $ readHexColor "#ffcfdc"
deepBlue              = fromJust $ readHexColor "#040273"
rust                  = fromJust $ readHexColor "#a83c09"
lightTeal             = fromJust $ readHexColor "#90e4c1"
slate                 = fromJust $ readHexColor "#516572"
goldenrod             = fromJust $ readHexColor "#fac205"
darkYellow            = fromJust $ readHexColor "#d5b60a"
darkGrey              = fromJust $ readHexColor "#363737"
armyGreen             = fromJust $ readHexColor "#4b5d16"
greyBlue              = fromJust $ readHexColor "#6b8ba4"
seafoam               = fromJust $ readHexColor "#80f9ad"
puce                  = fromJust $ readHexColor "#a57e52"
springGreen           = fromJust $ readHexColor "#a9f971"
darkOrange            = fromJust $ readHexColor "#c65102"
sand                  = fromJust $ readHexColor "#e2ca76"
pastelGreen           = fromJust $ readHexColor "#b0ff9d"
mint                  = fromJust $ readHexColor "#9ffeb0"
lightOrange           = fromJust $ readHexColor "#fdaa48"
brightPink            = fromJust $ readHexColor "#fe01b1"
chartreuse            = fromJust $ readHexColor "#c1f80a"
deepPurple            = fromJust $ readHexColor "#36013f"
darkBrown             = fromJust $ readHexColor "#341c02"
taupe                 = fromJust $ readHexColor "#b9a281"
peaGreen              = fromJust $ readHexColor "#8eab12"
pukeGreen             = fromJust $ readHexColor "#9aae07"
kellyGreen            = fromJust $ readHexColor "#02ab2e"
seafoamGreen          = fromJust $ readHexColor "#7af9ab"
blueGreen             = fromJust $ readHexColor "#137e6d"
khaki                 = fromJust $ readHexColor "#aaa662"
burgundy              = fromJust $ readHexColor "#610023"
darkTeal              = fromJust $ readHexColor "#014d4e"
brickRed              = fromJust $ readHexColor "#8f1402"
royalPurple           = fromJust $ readHexColor "#4b006e"
plum                  = fromJust $ readHexColor "#580f41"
mintGreen             = fromJust $ readHexColor "#8fff9f"
gold                  = fromJust $ readHexColor "#dbb40c"
babyBlue              = fromJust $ readHexColor "#a2cffe"
yellowGreen           = fromJust $ readHexColor "#c0fb2d"
brightPurple          = fromJust $ readHexColor "#be03fd"
darkRed               = fromJust $ readHexColor "#840000"
paleBlue              = fromJust $ readHexColor "#d0fefe"
grassGreen            = fromJust $ readHexColor "#3f9b0b"
navy                  = fromJust $ readHexColor "#01153e"
aquamarine            = fromJust $ readHexColor "#04d8b2"
burntOrange           = fromJust $ readHexColor "#c04e01"
neonGreen             = fromJust $ readHexColor "#0cff0c"
brightBlue            = fromJust $ readHexColor "#0165fc"
rose                  = fromJust $ readHexColor "#cf6275"
lightPink             = fromJust $ readHexColor "#ffd1df"
mustard               = fromJust $ readHexColor "#ceb301"
indigo                = fromJust $ readHexColor "#380282"
lime                  = fromJust $ readHexColor "#aaff32"
seaGreen              = fromJust $ readHexColor "#53fca1"
periwinkle            = fromJust $ readHexColor "#8e82fe"
darkPink              = fromJust $ readHexColor "#cb416b"
oliveGreen            = fromJust $ readHexColor "#677a04"
peach                 = fromJust $ readHexColor "#ffb07c"
paleGreen             = fromJust $ readHexColor "#c7fdb5"
lightBrown            = fromJust $ readHexColor "#ad8150"
hotPink               = fromJust $ readHexColor "#ff028d"
black                 = fromJust $ readHexColor "#000000"
lilac                 = fromJust $ readHexColor "#cea2fd"
navyBlue              = fromJust $ readHexColor "#001146"
royalBlue             = fromJust $ readHexColor "#0504aa"
beige                 = fromJust $ readHexColor "#e6daa6"
salmon                = fromJust $ readHexColor "#ff796c"
olive                 = fromJust $ readHexColor "#6e750e"
maroon                = fromJust $ readHexColor "#650021"
brightGreen           = fromJust $ readHexColor "#01ff07"
darkPurple            = fromJust $ readHexColor "#35063e"
mauve                 = fromJust $ readHexColor "#ae7181"
forestGreen           = fromJust $ readHexColor "#06470c"
aqua                  = fromJust $ readHexColor "#13eac9"
cyan                  = fromJust $ readHexColor "#00ffff"
tan                   = fromJust $ readHexColor "#d1b26f"
darkBlue              = fromJust $ readHexColor "#00035b"
lavender              = fromJust $ readHexColor "#c79fef"
turquoise             = fromJust $ readHexColor "#06c2ac"
darkGreen             = fromJust $ readHexColor "#033500"
violet                = fromJust $ readHexColor "#9a0eea"
lightPurple           = fromJust $ readHexColor "#bf77f6"
limeGreen             = fromJust $ readHexColor "#89fe05"
grey                  = fromJust $ readHexColor "#929591"
skyBlue               = fromJust $ readHexColor "#75bbfd"
yellow                = fromJust $ readHexColor "#ffff14"
magenta               = fromJust $ readHexColor "#c20078"
lightGreen            = fromJust $ readHexColor "#96f97b"
orange                = fromJust $ readHexColor "#f97306"
teal                  = fromJust $ readHexColor "#029386"
lightBlue             = fromJust $ readHexColor "#95d0fc"
red                   = fromJust $ readHexColor "#e50000"
brown                 = fromJust $ readHexColor "#653700"
pink                  = fromJust $ readHexColor "#ff81c0"
blue                  = fromJust $ readHexColor "#0343df"
green                 = fromJust $ readHexColor "#15b01a"
purple                = fromJust $ readHexColor "#7e1e9c"

xkcdColorMap :: M.Map String (AlphaColour Double)
xkcdColorMap = M.fromList
  [ ("cloudyBlue", cloudyBlue)
  , ("darkPastelGreen", darkPastelGreen)
  , ("dust", dust)
  , ("electricLime", electricLime)
  , ("freshGreen", freshGreen)
  , ("lightEggplant", lightEggplant)
  , ("nastyGreen", nastyGreen)
  , ("reallyLightBlue", reallyLightBlue)
  , ("tea", tea)
  , ("warmPurple", warmPurple)
  , ("yellowishTan", yellowishTan)
  , ("cement", cement)
  , ("darkGrassGreen", darkGrassGreen)
  , ("dustyTeal", dustyTeal)
  , ("greyTeal", greyTeal)
  , ("macaroniAndCheese", macaroniAndCheese)
  , ("pinkishTan", pinkishTan)
  , ("spruce", spruce)
  , ("strongBlue", strongBlue)
  , ("toxicGreen", toxicGreen)
  , ("windowsBlue", windowsBlue)
  , ("blueBlue", blueBlue)
  , ("blueWithAHintOfPurple", blueWithAHintOfPurple)
  , ("booger", booger)
  , ("brightSeaGreen", brightSeaGreen)
  , ("darkGreenBlue", darkGreenBlue)
  , ("deepTurquoise", deepTurquoise)
  , ("greenTeal", greenTeal)
  , ("strongPink", strongPink)
  , ("bland", bland)
  , ("deepAqua", deepAqua)
  , ("lavenderPink", lavenderPink)
  , ("lightMossGreen", lightMossGreen)
  , ("lightSeafoamGreen", lightSeafoamGreen)
  , ("oliveYellow", oliveYellow)
  , ("pigPink", pigPink)
  , ("deepLilac", deepLilac)
  , ("desert", desert)
  , ("dustyLavender", dustyLavender)
  , ("purpleyGrey", purpleyGrey)
  , ("purply", purply)
  , ("candyPink", candyPink)
  , ("lightPastelGreen", lightPastelGreen)
  , ("boringGreen", boringGreen)
  , ("kiwiGreen", kiwiGreen)
  , ("lightGreyGreen", lightGreyGreen)
  , ("orangePink", orangePink)
  , ("teaGreen", teaGreen)
  , ("veryLightBrown", veryLightBrown)
  , ("eggShell", eggShell)
  , ("eggplantPurple", eggplantPurple)
  , ("powderPink", powderPink)
  , ("reddishGrey", reddishGrey)
  , ("babyShitBrown", babyShitBrown)
  , ("liliac", liliac)
  , ("stormyBlue", stormyBlue)
  , ("uglyBrown", uglyBrown)
  , ("custard", custard)
  , ("darkishPink", darkishPink)
  , ("deepBrown", deepBrown)
  , ("greenishBeige", greenishBeige)
  , ("manilla", manilla)
  , ("offBlue", offBlue)
  , ("battleshipGrey", battleshipGrey)
  , ("brownyGreen", brownyGreen)
  , ("bruise", bruise)
  , ("kelleyGreen", kelleyGreen)
  , ("sicklyYellow", sicklyYellow)
  , ("sunnyYellow", sunnyYellow)
  , ("azul", azul)
  , ("darkgreen", darkgreen)
  , ("green_yellow", green_yellow)
  , ("lichen", lichen)
  , ("lightLightGreen", lightLightGreen)
  , ("paleGold", paleGold)
  , ("sunYellow", sunYellow)
  , ("tanGreen", tanGreen)
  , ("burple", burple)
  , ("butterscotch", butterscotch)
  , ("toupe", toupe)
  , ("darkCream", darkCream)
  , ("indianRed", indianRed)
  , ("lightLavendar", lightLavendar)
  , ("poisonGreen", poisonGreen)
  , ("babyPukeGreen", babyPukeGreen)
  , ("brightYellowGreen", brightYellowGreen)
  , ("charcoalGrey", charcoalGrey)
  , ("squash", squash)
  , ("cinnamon", cinnamon)
  , ("lightPeaGreen", lightPeaGreen)
  , ("radioactiveGreen", radioactiveGreen)
  , ("rawSienna", rawSienna)
  , ("babyPurple", babyPurple)
  , ("cocoa", cocoa)
  , ("lightRoyalBlue", lightRoyalBlue)
  , ("orangeish", orangeish)
  , ("rustBrown", rustBrown)
  , ("sandBrown", sandBrown)
  , ("swamp", swamp)
  , ("tealishGreen", tealishGreen)
  , ("burntSiena", burntSiena)
  , ("camo", camo)
  , ("duskBlue", duskBlue)
  , ("fern", fern)
  , ("oldRose", oldRose)
  , ("paleLightGreen", paleLightGreen)
  , ("peachyPink", peachyPink)
  , ("rosyPink", rosyPink)
  , ("lightBluishGreen", lightBluishGreen)
  , ("lightBrightGreen", lightBrightGreen)
  , ("lightNeonGreen", lightNeonGreen)
  , ("lightSeafoam", lightSeafoam)
  , ("tiffanyBlue", tiffanyBlue)
  , ("washedOutGreen", washedOutGreen)
  , ("brownyOrange", brownyOrange)
  , ("niceBlue", niceBlue)
  , ("sapphire", sapphire)
  , ("greyishTeal", greyishTeal)
  , ("orangeyYellow", orangeyYellow)
  , ("parchment", parchment)
  , ("straw", straw)
  , ("veryDarkBrown", veryDarkBrown)
  , ("terracota", terracota)
  , ("uglyBlue", uglyBlue)
  , ("clearBlue", clearBlue)
  , ("creme", creme)
  , ("foamGreen", foamGreen)
  , ("grey_green", grey_green)
  , ("lightGold", lightGold)
  , ("seafoamBlue", seafoamBlue)
  , ("topaz", topaz)
  , ("violetPink", violetPink)
  , ("wintergreen", wintergreen)
  , ("yellowTan", yellowTan)
  , ("darkFuchsia", darkFuchsia)
  , ("indigoBlue", indigoBlue)
  , ("lightYellowishGreen", lightYellowishGreen)
  , ("paleMagenta", paleMagenta)
  , ("richPurple", richPurple)
  , ("sunflowerYellow", sunflowerYellow)
  , ("green_blue", green_blue)
  , ("leather", leather)
  , ("racingGreen", racingGreen)
  , ("vividPurple", vividPurple)
  , ("darkRoyalBlue", darkRoyalBlue)
  , ("hazel", hazel)
  , ("mutedPink", mutedPink)
  , ("boogerGreen", boogerGreen)
  , ("canary", canary)
  , ("coolGrey", coolGrey)
  , ("darkTaupe", darkTaupe)
  , ("darkishPurple", darkishPurple)
  , ("trueGreen", trueGreen)
  , ("coralPink", coralPink)
  , ("darkSage", darkSage)
  , ("darkSlateBlue", darkSlateBlue)
  , ("flatBlue", flatBlue)
  , ("mushroom", mushroom)
  , ("richBlue", richBlue)
  , ("dirtyPurple", dirtyPurple)
  , ("greenblue", greenblue)
  , ("ickyGreen", ickyGreen)
  , ("lightKhaki", lightKhaki)
  , ("warmBlue", warmBlue)
  , ("darkHotPink", darkHotPink)
  , ("deepSeaBlue", deepSeaBlue)
  , ("carmine", carmine)
  , ("darkYellowGreen", darkYellowGreen)
  , ("palePeach", palePeach)
  , ("plumPurple", plumPurple)
  , ("goldenRod", goldenRod)
  , ("neonRed", neonRed)
  , ("oldPink", oldPink)
  , ("veryPaleBlue", veryPaleBlue)
  , ("bloodOrange", bloodOrange)
  , ("grapefruit", grapefruit)
  , ("sandYellow", sandYellow)
  , ("clayBrown", clayBrown)
  , ("darkBlueGrey", darkBlueGrey)
  , ("flatGreen", flatGreen)
  , ("lightGreenBlue", lightGreenBlue)
  , ("warmPink", warmPink)
  , ("dodgerBlue", dodgerBlue)
  , ("grossGreen", grossGreen)
  , ("ice", ice)
  , ("metallicBlue", metallicBlue)
  , ("paleSalmon", paleSalmon)
  , ("sapGreen", sapGreen)
  , ("algae", algae)
  , ("blueyGrey", blueyGrey)
  , ("greenyGrey", greenyGrey)
  , ("highlighterGreen", highlighterGreen)
  , ("lightLightBlue", lightLightBlue)
  , ("lightMint", lightMint)
  , ("rawUmber", rawUmber)
  , ("vividBlue", vividBlue)
  , ("deepLavender", deepLavender)
  , ("dullTeal", dullTeal)
  , ("lightGreenishBlue", lightGreenishBlue)
  , ("mudGreen", mudGreen)
  , ("pinky", pinky)
  , ("redWine", redWine)
  , ("shitGreen", shitGreen)
  , ("tanBrown", tanBrown)
  , ("darkblue", darkblue)
  , ("rosa", rosa)
  , ("lipstick", lipstick)
  , ("paleMauve", paleMauve)
  , ("claret", claret)
  , ("dandelion", dandelion)
  , ("orangered", orangered)
  , ("poopGreen", poopGreen)
  , ("ruby", ruby)
  , ("dark", dark)
  , ("greenishTurquoise", greenishTurquoise)
  , ("pastelRed", pastelRed)
  , ("pissYellow", pissYellow)
  , ("brightCyan", brightCyan)
  , ("darkCoral", darkCoral)
  , ("algaeGreen", algaeGreen)
  , ("darkishRed", darkishRed)
  , ("reddyBrown", reddyBrown)
  , ("blushPink", blushPink)
  , ("camouflageGreen", camouflageGreen)
  , ("lawnGreen", lawnGreen)
  , ("putty", putty)
  , ("vibrantBlue", vibrantBlue)
  , ("darkSand", darkSand)
  , ("purple_blue", purple_blue)
  , ("saffron", saffron)
  , ("twilight", twilight)
  , ("warmBrown", warmBrown)
  , ("bluegrey", bluegrey)
  , ("bubbleGumPink", bubbleGumPink)
  , ("duckEggBlue", duckEggBlue)
  , ("greenishCyan", greenishCyan)
  , ("petrol", petrol)
  , ("royal", royal)
  , ("butter", butter)
  , ("dustyOrange", dustyOrange)
  , ("offYellow", offYellow)
  , ("paleOliveGreen", paleOliveGreen)
  , ("orangish", orangish)
  , ("leaf", leaf)
  , ("lightBlueGrey", lightBlueGrey)
  , ("driedBlood", driedBlood)
  , ("lightishPurple", lightishPurple)
  , ("rustyRed", rustyRed)
  , ("lavenderBlue", lavenderBlue)
  , ("lightGrassGreen", lightGrassGreen)
  , ("lightMintGreen", lightMintGreen)
  , ("sunflower", sunflower)
  , ("velvet", velvet)
  , ("brickOrange", brickOrange)
  , ("lightishRed", lightishRed)
  , ("pureBlue", pureBlue)
  , ("twilightBlue", twilightBlue)
  , ("violetRed", violetRed)
  , ("yellowyBrown", yellowyBrown)
  , ("carnation", carnation)
  , ("muddyYellow", muddyYellow)
  , ("darkSeafoamGreen", darkSeafoamGreen)
  , ("deepRose", deepRose)
  , ("dustyRed", dustyRed)
  , ("grey_blue", grey_blue)
  , ("lemonLime", lemonLime)
  , ("purple_pink", purple_pink)
  , ("brownYellow", brownYellow)
  , ("purpleBrown", purpleBrown)
  , ("wisteria", wisteria)
  , ("bananaYellow", bananaYellow)
  , ("lipstickRed", lipstickRed)
  , ("waterBlue", waterBlue)
  , ("brownGrey", brownGrey)
  , ("vibrantPurple", vibrantPurple)
  , ("babyGreen", babyGreen)
  , ("barfGreen", barfGreen)
  , ("eggshellBlue", eggshellBlue)
  , ("sandyYellow", sandyYellow)
  , ("coolGreen", coolGreen)
  , ("pale", pale)
  , ("blue_grey", blue_grey)
  , ("hotMagenta", hotMagenta)
  , ("greyblue", greyblue)
  , ("purpley", purpley)
  , ("babyShitGreen", babyShitGreen)
  , ("brownishPink", brownishPink)
  , ("darkAquamarine", darkAquamarine)
  , ("diarrhea", diarrhea)
  , ("lightMustard", lightMustard)
  , ("paleSkyBlue", paleSkyBlue)
  , ("turtleGreen", turtleGreen)
  , ("brightOlive", brightOlive)
  , ("darkGreyBlue", darkGreyBlue)
  , ("greenyBrown", greenyBrown)
  , ("lemonGreen", lemonGreen)
  , ("lightPeriwinkle", lightPeriwinkle)
  , ("seaweedGreen", seaweedGreen)
  , ("sunshineYellow", sunshineYellow)
  , ("uglyPurple", uglyPurple)
  , ("mediumPink", mediumPink)
  , ("pukeBrown", pukeBrown)
  , ("veryLightPink", veryLightPink)
  , ("viridian", viridian)
  , ("bile", bile)
  , ("fadedYellow", fadedYellow)
  , ("veryPaleGreen", veryPaleGreen)
  , ("vibrantGreen", vibrantGreen)
  , ("brightLime", brightLime)
  , ("spearmint", spearmint)
  , ("lightAquamarine", lightAquamarine)
  , ("lightSage", lightSage)
  , ("yellowgreen", yellowgreen)
  , ("babyPoo", babyPoo)
  , ("darkSeafoam", darkSeafoam)
  , ("deepTeal", deepTeal)
  , ("heather", heather)
  , ("rustOrange", rustOrange)
  , ("dirtyBlue", dirtyBlue)
  , ("fernGreen", fernGreen)
  , ("brightLilac", brightLilac)
  , ("weirdGreen", weirdGreen)
  , ("peacockBlue", peacockBlue)
  , ("avocadoGreen", avocadoGreen)
  , ("fadedOrange", fadedOrange)
  , ("grapePurple", grapePurple)
  , ("hotGreen", hotGreen)
  , ("limeYellow", limeYellow)
  , ("mango", mango)
  , ("shamrock", shamrock)
  , ("bubblegum", bubblegum)
  , ("purplishBrown", purplishBrown)
  , ("vomitYellow", vomitYellow)
  , ("paleCyan", paleCyan)
  , ("keyLime", keyLime)
  , ("tomatoRed", tomatoRed)
  , ("lightgreen", lightgreen)
  , ("merlot", merlot)
  , ("nightBlue", nightBlue)
  , ("purpleishPink", purpleishPink)
  , ("apple", apple)
  , ("babyPoopGreen", babyPoopGreen)
  , ("greenApple", greenApple)
  , ("heliotrope", heliotrope)
  , ("yellow_green", yellow_green)
  , ("almostBlack", almostBlack)
  , ("coolBlue", coolBlue)
  , ("leafyGreen", leafyGreen)
  , ("mustardBrown", mustardBrown)
  , ("dusk", dusk)
  , ("dullBrown", dullBrown)
  , ("frogGreen", frogGreen)
  , ("vividGreen", vividGreen)
  , ("brightLightGreen", brightLightGreen)
  , ("fluroGreen", fluroGreen)
  , ("kiwi", kiwi)
  , ("seaweed", seaweed)
  , ("navyGreen", navyGreen)
  , ("ultramarineBlue", ultramarineBlue)
  , ("iris", iris)
  , ("pastelOrange", pastelOrange)
  , ("yellowishOrange", yellowishOrange)
  , ("perrywinkle", perrywinkle)
  , ("tealish", tealish)
  , ("darkPlum", darkPlum)
  , ("pear", pear)
  , ("pinkishOrange", pinkishOrange)
  , ("midnightPurple", midnightPurple)
  , ("lightUrple", lightUrple)
  , ("darkMint", darkMint)
  , ("greenishTan", greenishTan)
  , ("lightBurgundy", lightBurgundy)
  , ("turquoiseBlue", turquoiseBlue)
  , ("uglyPink", uglyPink)
  , ("sandy", sandy)
  , ("electricPink", electricPink)
  , ("mutedPurple", mutedPurple)
  , ("midGreen", midGreen)
  , ("greyish", greyish)
  , ("neonYellow", neonYellow)
  , ("banana", banana)
  , ("carnationPink", carnationPink)
  , ("tomato", tomato)
  , ("sea", sea)
  , ("muddyBrown", muddyBrown)
  , ("turquoiseGreen", turquoiseGreen)
  , ("buff", buff)
  , ("fawn", fawn)
  , ("mutedBlue", mutedBlue)
  , ("paleRose", paleRose)
  , ("darkMintGreen", darkMintGreen)
  , ("amethyst", amethyst)
  , ("blue_green", blue_green)
  , ("chestnut", chestnut)
  , ("sickGreen", sickGreen)
  , ("pea", pea)
  , ("rustyOrange", rustyOrange)
  , ("stone", stone)
  , ("roseRed", roseRed)
  , ("paleAqua", paleAqua)
  , ("deepOrange", deepOrange)
  , ("earth", earth)
  , ("mossyGreen", mossyGreen)
  , ("grassyGreen", grassyGreen)
  , ("paleLimeGreen", paleLimeGreen)
  , ("lightGreyBlue", lightGreyBlue)
  , ("paleGrey", paleGrey)
  , ("asparagus", asparagus)
  , ("blueberry", blueberry)
  , ("purpleRed", purpleRed)
  , ("paleLime", paleLime)
  , ("greenishTeal", greenishTeal)
  , ("caramel", caramel)
  , ("deepMagenta", deepMagenta)
  , ("lightPeach", lightPeach)
  , ("milkChocolate", milkChocolate)
  , ("ocher", ocher)
  , ("offGreen", offGreen)
  , ("purplyPink", purplyPink)
  , ("lightblue", lightblue)
  , ("duskyBlue", duskyBlue)
  , ("golden", golden)
  , ("lightBeige", lightBeige)
  , ("butterYellow", butterYellow)
  , ("duskyPurple", duskyPurple)
  , ("frenchBlue", frenchBlue)
  , ("uglyYellow", uglyYellow)
  , ("greenyYellow", greenyYellow)
  , ("orangishRed", orangishRed)
  , ("shamrockGreen", shamrockGreen)
  , ("orangishBrown", orangishBrown)
  , ("treeGreen", treeGreen)
  , ("deepViolet", deepViolet)
  , ("gunmetal", gunmetal)
  , ("blue_purple", blue_purple)
  , ("cherry", cherry)
  , ("sandyBrown", sandyBrown)
  , ("warmGrey", warmGrey)
  , ("darkIndigo", darkIndigo)
  , ("midnight", midnight)
  , ("blueyGreen", blueyGreen)
  , ("greyPink", greyPink)
  , ("softPurple", softPurple)
  , ("blood", blood)
  , ("brownRed", brownRed)
  , ("mediumGrey", mediumGrey)
  , ("berry", berry)
  , ("poo", poo)
  , ("purpleyPink", purpleyPink)
  , ("lightSalmon", lightSalmon)
  , ("snot", snot)
  , ("easterPurple", easterPurple)
  , ("lightYellowGreen", lightYellowGreen)
  , ("darkNavyBlue", darkNavyBlue)
  , ("drab", drab)
  , ("lightRose", lightRose)
  , ("rouge", rouge)
  , ("purplishRed", purplishRed)
  , ("slimeGreen", slimeGreen)
  , ("babyPoop", babyPoop)
  , ("irishGreen", irishGreen)
  , ("pink_purple", pink_purple)
  , ("darkNavy", darkNavy)
  , ("greenyBlue", greenyBlue)
  , ("lightPlum", lightPlum)
  , ("pinkishGrey", pinkishGrey)
  , ("dirtyOrange", dirtyOrange)
  , ("rustRed", rustRed)
  , ("paleLilac", paleLilac)
  , ("orangeyRed", orangeyRed)
  , ("primaryBlue", primaryBlue)
  , ("kermitGreen", kermitGreen)
  , ("brownishPurple", brownishPurple)
  , ("murkyGreen", murkyGreen)
  , ("wheat", wheat)
  , ("veryDarkPurple", veryDarkPurple)
  , ("bottleGreen", bottleGreen)
  , ("watermelon", watermelon)
  , ("deepSkyBlue", deepSkyBlue)
  , ("fireEngineRed", fireEngineRed)
  , ("yellowOchre", yellowOchre)
  , ("pumpkinOrange", pumpkinOrange)
  , ("paleOlive", paleOlive)
  , ("lightLilac", lightLilac)
  , ("lightishGreen", lightishGreen)
  , ("carolinaBlue", carolinaBlue)
  , ("mulberry", mulberry)
  , ("shockingPink", shockingPink)
  , ("auburn", auburn)
  , ("brightLimeGreen", brightLimeGreen)
  , ("celadon", celadon)
  , ("pinkishBrown", pinkishBrown)
  , ("pooBrown", pooBrown)
  , ("brightSkyBlue", brightSkyBlue)
  , ("celery", celery)
  , ("dirtBrown", dirtBrown)
  , ("strawberry", strawberry)
  , ("darkLime", darkLime)
  , ("copper", copper)
  , ("mediumBrown", mediumBrown)
  , ("mutedGreen", mutedGreen)
  , ("robin'sEgg", robin'sEgg)
  , ("brightAqua", brightAqua)
  , ("brightLavender", brightLavender)
  , ("ivory", ivory)
  , ("veryLightPurple", veryLightPurple)
  , ("lightNavy", lightNavy)
  , ("pinkRed", pinkRed)
  , ("oliveBrown", oliveBrown)
  , ("poopBrown", poopBrown)
  , ("mustardGreen", mustardGreen)
  , ("oceanGreen", oceanGreen)
  , ("veryDarkBlue", veryDarkBlue)
  , ("dustyGreen", dustyGreen)
  , ("lightNavyBlue", lightNavyBlue)
  , ("mintyGreen", mintyGreen)
  , ("adobe", adobe)
  , ("barney", barney)
  , ("jadeGreen", jadeGreen)
  , ("brightLightBlue", brightLightBlue)
  , ("lightLime", lightLime)
  , ("darkKhaki", darkKhaki)
  , ("orangeYellow", orangeYellow)
  , ("ocre", ocre)
  , ("maize", maize)
  , ("fadedPink", fadedPink)
  , ("britishRacingGreen", britishRacingGreen)
  , ("sandstone", sandstone)
  , ("mudBrown", mudBrown)
  , ("lightSeaGreen", lightSeaGreen)
  , ("robinEggBlue", robinEggBlue)
  , ("aquaMarine", aquaMarine)
  , ("darkSeaGreen", darkSeaGreen)
  , ("softPink", softPink)
  , ("orangeyBrown", orangeyBrown)
  , ("cherryRed", cherryRed)
  , ("burntYellow", burntYellow)
  , ("brownishGrey", brownishGrey)
  , ("camel", camel)
  , ("purplishGrey", purplishGrey)
  , ("marine", marine)
  , ("greyishPink", greyishPink)
  , ("paleTurquoise", paleTurquoise)
  , ("pastelYellow", pastelYellow)
  , ("blueyPurple", blueyPurple)
  , ("canaryYellow", canaryYellow)
  , ("fadedRed", fadedRed)
  , ("sepia", sepia)
  , ("coffee", coffee)
  , ("brightMagenta", brightMagenta)
  , ("mocha", mocha)
  , ("ecru", ecru)
  , ("purpleish", purpleish)
  , ("cranberry", cranberry)
  , ("darkishGreen", darkishGreen)
  , ("brownOrange", brownOrange)
  , ("duskyRose", duskyRose)
  , ("melon", melon)
  , ("sicklyGreen", sicklyGreen)
  , ("silver", silver)
  , ("purplyBlue", purplyBlue)
  , ("purpleishBlue", purpleishBlue)
  , ("hospitalGreen", hospitalGreen)
  , ("shitBrown", shitBrown)
  , ("midBlue", midBlue)
  , ("amber", amber)
  , ("easterGreen", easterGreen)
  , ("softBlue", softBlue)
  , ("ceruleanBlue", ceruleanBlue)
  , ("goldenBrown", goldenBrown)
  , ("brightTurquoise", brightTurquoise)
  , ("redPink", redPink)
  , ("redPurple", redPurple)
  , ("greyishBrown", greyishBrown)
  , ("vermillion", vermillion)
  , ("russet", russet)
  , ("steelGrey", steelGrey)
  , ("lighterPurple", lighterPurple)
  , ("brightViolet", brightViolet)
  , ("prussianBlue", prussianBlue)
  , ("slateGreen", slateGreen)
  , ("dirtyPink", dirtyPink)
  , ("darkBlueGreen", darkBlueGreen)
  , ("pine", pine)
  , ("yellowyGreen", yellowyGreen)
  , ("darkGold", darkGold)
  , ("bluish", bluish)
  , ("darkishBlue", darkishBlue)
  , ("dullRed", dullRed)
  , ("pinkyRed", pinkyRed)
  , ("bronze", bronze)
  , ("paleTeal", paleTeal)
  , ("militaryGreen", militaryGreen)
  , ("barbiePink", barbiePink)
  , ("bubblegumPink", bubblegumPink)
  , ("peaSoupGreen", peaSoupGreen)
  , ("darkMustard", darkMustard)
  , ("shit", shit)
  , ("mediumPurple", mediumPurple)
  , ("veryDarkGreen", veryDarkGreen)
  , ("dirt", dirt)
  , ("duskyPink", duskyPink)
  , ("redViolet", redViolet)
  , ("lemonYellow", lemonYellow)
  , ("pistachio", pistachio)
  , ("dullYellow", dullYellow)
  , ("darkLimeGreen", darkLimeGreen)
  , ("denimBlue", denimBlue)
  , ("tealBlue", tealBlue)
  , ("lightishBlue", lightishBlue)
  , ("purpleyBlue", purpleyBlue)
  , ("lightIndigo", lightIndigo)
  , ("swampGreen", swampGreen)
  , ("brownGreen", brownGreen)
  , ("darkMaroon", darkMaroon)
  , ("hotPurple", hotPurple)
  , ("darkForestGreen", darkForestGreen)
  , ("fadedBlue", fadedBlue)
  , ("drabGreen", drabGreen)
  , ("lightLimeGreen", lightLimeGreen)
  , ("snotGreen", snotGreen)
  , ("yellowish", yellowish)
  , ("lightBlueGreen", lightBlueGreen)
  , ("bordeaux", bordeaux)
  , ("lightMauve", lightMauve)
  , ("ocean", ocean)
  , ("marigold", marigold)
  , ("muddyGreen", muddyGreen)
  , ("dullOrange", dullOrange)
  , ("steel", steel)
  , ("electricPurple", electricPurple)
  , ("fluorescentGreen", fluorescentGreen)
  , ("yellowishBrown", yellowishBrown)
  , ("blush", blush)
  , ("softGreen", softGreen)
  , ("brightOrange", brightOrange)
  , ("lemon", lemon)
  , ("purpleGrey", purpleGrey)
  , ("acidGreen", acidGreen)
  , ("paleLavender", paleLavender)
  , ("violetBlue", violetBlue)
  , ("lightForestGreen", lightForestGreen)
  , ("burntRed", burntRed)
  , ("khakiGreen", khakiGreen)
  , ("cerise", cerise)
  , ("fadedPurple", fadedPurple)
  , ("apricot", apricot)
  , ("darkOliveGreen", darkOliveGreen)
  , ("greyBrown", greyBrown)
  , ("greenGrey", greenGrey)
  , ("trueBlue", trueBlue)
  , ("paleViolet", paleViolet)
  , ("periwinkleBlue", periwinkleBlue)
  , ("lightSkyBlue", lightSkyBlue)
  , ("blurple", blurple)
  , ("greenBrown", greenBrown)
  , ("bluegreen", bluegreen)
  , ("brightTeal", brightTeal)
  , ("brownishYellow", brownishYellow)
  , ("peaSoup", peaSoup)
  , ("forest", forest)
  , ("barneyPurple", barneyPurple)
  , ("ultramarine", ultramarine)
  , ("purplish", purplish)
  , ("pukeYellow", pukeYellow)
  , ("bluishGrey", bluishGrey)
  , ("darkPeriwinkle", darkPeriwinkle)
  , ("darkLilac", darkLilac)
  , ("reddish", reddish)
  , ("lightMaroon", lightMaroon)
  , ("dustyPurple", dustyPurple)
  , ("terraCotta", terraCotta)
  , ("avocado", avocado)
  , ("marineBlue", marineBlue)
  , ("tealGreen", tealGreen)
  , ("slateGrey", slateGrey)
  , ("lighterGreen", lighterGreen)
  , ("electricGreen", electricGreen)
  , ("dustyBlue", dustyBlue)
  , ("goldenYellow", goldenYellow)
  , ("brightYellow", brightYellow)
  , ("lightLavender", lightLavender)
  , ("umber", umber)
  , ("poop", poop)
  , ("darkPeach", darkPeach)
  , ("jungleGreen", jungleGreen)
  , ("eggshell", eggshell)
  , ("denim", denim)
  , ("yellowBrown", yellowBrown)
  , ("dullPurple", dullPurple)
  , ("chocolateBrown", chocolateBrown)
  , ("wineRed", wineRed)
  , ("neonBlue", neonBlue)
  , ("dirtyGreen", dirtyGreen)
  , ("lightTan", lightTan)
  , ("iceBlue", iceBlue)
  , ("cadetBlue", cadetBlue)
  , ("darkMauve", darkMauve)
  , ("veryLightBlue", veryLightBlue)
  , ("greyPurple", greyPurple)
  , ("pastelPink", pastelPink)
  , ("veryLightGreen", veryLightGreen)
  , ("darkSkyBlue", darkSkyBlue)
  , ("evergreen", evergreen)
  , ("dullPink", dullPink)
  , ("aubergine", aubergine)
  , ("mahogany", mahogany)
  , ("reddishOrange", reddishOrange)
  , ("deepGreen", deepGreen)
  , ("vomitGreen", vomitGreen)
  , ("purplePink", purplePink)
  , ("dustyPink", dustyPink)
  , ("fadedGreen", fadedGreen)
  , ("camoGreen", camoGreen)
  , ("pinkyPurple", pinkyPurple)
  , ("pinkPurple", pinkPurple)
  , ("brownishRed", brownishRed)
  , ("darkRose", darkRose)
  , ("mud", mud)
  , ("brownish", brownish)
  , ("emeraldGreen", emeraldGreen)
  , ("paleBrown", paleBrown)
  , ("dullBlue", dullBlue)
  , ("burntUmber", burntUmber)
  , ("mediumGreen", mediumGreen)
  , ("clay", clay)
  , ("lightAqua", lightAqua)
  , ("lightOliveGreen", lightOliveGreen)
  , ("brownishOrange", brownishOrange)
  , ("darkAqua", darkAqua)
  , ("purplishPink", purplishPink)
  , ("darkSalmon", darkSalmon)
  , ("greenishGrey", greenishGrey)
  , ("jade", jade)
  , ("uglyGreen", uglyGreen)
  , ("darkBeige", darkBeige)
  , ("emerald", emerald)
  , ("paleRed", paleRed)
  , ("lightMagenta", lightMagenta)
  , ("sky", sky)
  , ("lightCyan", lightCyan)
  , ("yellowOrange", yellowOrange)
  , ("reddishPurple", reddishPurple)
  , ("reddishPink", reddishPink)
  , ("orchid", orchid)
  , ("dirtyYellow", dirtyYellow)
  , ("orangeRed", orangeRed)
  , ("deepRed", deepRed)
  , ("orangeBrown", orangeBrown)
  , ("cobaltBlue", cobaltBlue)
  , ("neonPink", neonPink)
  , ("rosePink", rosePink)
  , ("greyishPurple", greyishPurple)
  , ("raspberry", raspberry)
  , ("aquaGreen", aquaGreen)
  , ("salmonPink", salmonPink)
  , ("tangerine", tangerine)
  , ("brownishGreen", brownishGreen)
  , ("redBrown", redBrown)
  , ("greenishBrown", greenishBrown)
  , ("pumpkin", pumpkin)
  , ("pineGreen", pineGreen)
  , ("charcoal", charcoal)
  , ("babyPink", babyPink)
  , ("cornflower", cornflower)
  , ("blueViolet", blueViolet)
  , ("chocolate", chocolate)
  , ("greyishGreen", greyishGreen)
  , ("scarlet", scarlet)
  , ("greenYellow", greenYellow)
  , ("darkOlive", darkOlive)
  , ("sienna", sienna)
  , ("pastelPurple", pastelPurple)
  , ("terracotta", terracotta)
  , ("aquaBlue", aquaBlue)
  , ("sageGreen", sageGreen)
  , ("bloodRed", bloodRed)
  , ("deepPink", deepPink)
  , ("grass", grass)
  , ("moss", moss)
  , ("pastelBlue", pastelBlue)
  , ("bluishGreen", bluishGreen)
  , ("greenBlue", greenBlue)
  , ("darkTan", darkTan)
  , ("greenishBlue", greenishBlue)
  , ("paleOrange", paleOrange)
  , ("vomit", vomit)
  , ("forrestGreen", forrestGreen)
  , ("darkLavender", darkLavender)
  , ("darkViolet", darkViolet)
  , ("purpleBlue", purpleBlue)
  , ("darkCyan", darkCyan)
  , ("oliveDrab", oliveDrab)
  , ("pinkish", pinkish)
  , ("cobalt", cobalt)
  , ("neonPurple", neonPurple)
  , ("lightTurquoise", lightTurquoise)
  , ("appleGreen", appleGreen)
  , ("dullGreen", dullGreen)
  , ("wine", wine)
  , ("powderBlue", powderBlue)
  , ("offWhite", offWhite)
  , ("electricBlue", electricBlue)
  , ("darkTurquoise", darkTurquoise)
  , ("bluePurple", bluePurple)
  , ("azure", azure)
  , ("brightRed", brightRed)
  , ("pinkishRed", pinkishRed)
  , ("cornflowerBlue", cornflowerBlue)
  , ("lightOlive", lightOlive)
  , ("grape", grape)
  , ("greyishBlue", greyishBlue)
  , ("purplishBlue", purplishBlue)
  , ("yellowishGreen", yellowishGreen)
  , ("greenishYellow", greenishYellow)
  , ("mediumBlue", mediumBlue)
  , ("dustyRose", dustyRose)
  , ("lightViolet", lightViolet)
  , ("midnightBlue", midnightBlue)
  , ("bluishPurple", bluishPurple)
  , ("redOrange", redOrange)
  , ("darkMagenta", darkMagenta)
  , ("greenish", greenish)
  , ("oceanBlue", oceanBlue)
  , ("coral", coral)
  , ("cream", cream)
  , ("reddishBrown", reddishBrown)
  , ("burntSienna", burntSienna)
  , ("brick", brick)
  , ("sage", sage)
  , ("greyGreen", greyGreen)
  , ("white", white)
  , ("robin'sEggBlue", robin'sEggBlue)
  , ("mossGreen", mossGreen)
  , ("steelBlue", steelBlue)
  , ("eggplant", eggplant)
  , ("lightYellow", lightYellow)
  , ("leafGreen", leafGreen)
  , ("lightGrey", lightGrey)
  , ("puke", puke)
  , ("pinkishPurple", pinkishPurple)
  , ("seaBlue", seaBlue)
  , ("palePurple", palePurple)
  , ("slateBlue", slateBlue)
  , ("blueGrey", blueGrey)
  , ("hunterGreen", hunterGreen)
  , ("fuchsia", fuchsia)
  , ("crimson", crimson)
  , ("paleYellow", paleYellow)
  , ("ochre", ochre)
  , ("mustardYellow", mustardYellow)
  , ("lightRed", lightRed)
  , ("cerulean", cerulean)
  , ("palePink", palePink)
  , ("deepBlue", deepBlue)
  , ("rust", rust)
  , ("lightTeal", lightTeal)
  , ("slate", slate)
  , ("goldenrod", goldenrod)
  , ("darkYellow", darkYellow)
  , ("darkGrey", darkGrey)
  , ("armyGreen", armyGreen)
  , ("greyBlue", greyBlue)
  , ("seafoam", seafoam)
  , ("puce", puce)
  , ("springGreen", springGreen)
  , ("darkOrange", darkOrange)
  , ("sand", sand)
  , ("pastelGreen", pastelGreen)
  , ("mint", mint)
  , ("lightOrange", lightOrange)
  , ("brightPink", brightPink)
  , ("chartreuse", chartreuse)
  , ("deepPurple", deepPurple)
  , ("darkBrown", darkBrown)
  , ("taupe", taupe)
  , ("peaGreen", peaGreen)
  , ("pukeGreen", pukeGreen)
  , ("kellyGreen", kellyGreen)
  , ("seafoamGreen", seafoamGreen)
  , ("blueGreen", blueGreen)
  , ("khaki", khaki)
  , ("burgundy", burgundy)
  , ("darkTeal", darkTeal)
  , ("brickRed", brickRed)
  , ("royalPurple", royalPurple)
  , ("plum", plum)
  , ("mintGreen", mintGreen)
  , ("gold", gold)
  , ("babyBlue", babyBlue)
  , ("yellowGreen", yellowGreen)
  , ("brightPurple", brightPurple)
  , ("darkRed", darkRed)
  , ("paleBlue", paleBlue)
  , ("grassGreen", grassGreen)
  , ("navy", navy)
  , ("aquamarine", aquamarine)
  , ("burntOrange", burntOrange)
  , ("neonGreen", neonGreen)
  , ("brightBlue", brightBlue)
  , ("rose", rose)
  , ("lightPink", lightPink)
  , ("mustard", mustard)
  , ("indigo", indigo)
  , ("lime", lime)
  , ("seaGreen", seaGreen)
  , ("periwinkle", periwinkle)
  , ("darkPink", darkPink)
  , ("oliveGreen", oliveGreen)
  , ("peach", peach)
  , ("paleGreen", paleGreen)
  , ("lightBrown", lightBrown)
  , ("hotPink", hotPink)
  , ("black", black)
  , ("lilac", lilac)
  , ("navyBlue", navyBlue)
  , ("royalBlue", royalBlue)
  , ("beige", beige)
  , ("salmon", salmon)
  , ("olive", olive)
  , ("maroon", maroon)
  , ("brightGreen", brightGreen)
  , ("darkPurple", darkPurple)
  , ("mauve", mauve)
  , ("forestGreen", forestGreen)
  , ("aqua", aqua)
  , ("cyan", cyan)
  , ("tan", tan)
  , ("darkBlue", darkBlue)
  , ("lavender", lavender)
  , ("turquoise", turquoise)
  , ("darkGreen", darkGreen)
  , ("violet", violet)
  , ("lightPurple", lightPurple)
  , ("limeGreen", limeGreen)
  , ("grey", grey)
  , ("skyBlue", skyBlue)
  , ("yellow", yellow)
  , ("magenta", magenta)
  , ("lightGreen", lightGreen)
  , ("orange", orange)
  , ("teal", teal)
  , ("lightBlue", lightBlue)
  , ("red", red)
  , ("brown", brown)
  , ("pink", pink)
  , ("blue", blue)
  , ("green", green)
  , ("purple", purple)
  ]
