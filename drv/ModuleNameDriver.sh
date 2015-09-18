#

if [[ -z $SWIMUNIT_ROOT ]]
then
  echo "[FATAL] Environment variable SWIMUNIT_ROOT is not set."
  echo "        Remedy: Set SWIMUNIT_ROOT to directory of Swimunit sources."
  exit -1
fi

OUTPUT=$SWIMUNIT_ROOT/output/drv

XRES=999
YRES=999

cabal run SwimunitModuleNameDriver  -- --width $XRES --height $YRES --output $OUTPUT/SwimunitModuleName.svg
cabal run DotmatrixModuleNameDriver -- --width $XRES --height $YRES --output $OUTPUT/DotmatrixModuleName.svg

 ##
####
 ##
