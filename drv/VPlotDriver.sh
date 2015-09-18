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

cabal --config-file=$SWIMUNIT_ROOT/swimunit.cabal run VPlotDriver -- --width $XRES --height $YRES --output $OUTPUT/VPlot.svg

 ##
####
 ##
