# Set environment variables.  Note this should be run with 'source'
# rather than executed, so the environment variables are set in the
# parent shell.

# aeson is not yet part of the Platform, but the constrained version
# is needed to build on with platform versions of some dependencies
# (at least attoparsec)

date

case "$HPVER" in
"") ;;
"2014.2.0.0")
  export GHCVER=7.8.3 ;
  echo "constraints:async==2.0.1.5,attoparsec==0.10.4.0,case-insensitive==1.1.0.3,fgl==5.5.0.1,GLUT==2.5.1.1,GLURaw==1.4.0.1,haskell-src==1.0.1.6,hashable==1.2.2.0,html==1.0.1.2,HTTP==4000.2.10,HUnit==1.2.5.2,mtl==2.1.3.1,network==2.4.2.3,OpenGL==2.9.2.0,OpenGLRaw==1.5.0.0,parallel==3.2.0.4,parsec==3.1.5,primitive==0.5.2.1,QuickCheck==2.6,random==1.0.1.1,regex-base==0.93.2,regex-compat==0.95.1,regex-posix==0.95.2,split==0.2.2,stm==2.4.2,syb==0.4.1,text==1.1.0.0,transformers==0.3.0.0,unordered-containers==0.2.4.0,vector==0.10.9.1,xhtml==3000.2.1,zlib==0.5.4.1" > cabal.config ;;

"2013.2.0.0")
  export GHCVER=7.6.3 ;
  echo "constraints:async==2.0.1.4,attoparsec==0.10.4.0,case-insensitive==1.0.0.1,cgi==3001.1.7.5,fgl==5.4.2.4,GLUT==2.4.0.0,GLURaw==1.3.0.0,haskell-src==1.0.1.5,hashable==1.1.2.5,html==1.0.1.2,HTTP==4000.2.8,HUnit==1.2.5.2,mtl==2.1.2,network==2.4.1.2,OpenGL==2.8.0.0,OpenGLRaw==1.3.0.0,parallel==3.2.0.3,parsec==3.1.3,QuickCheck==2.6,random==1.0.1.1,regex-base==0.93.2,regex-compat==0.95.1,regex-posix==0.95.2,split==0.2.2,stm==2.4.2,syb==0.4.0,text==0.11.3.1,transformers==0.3.0.0,unordered-containers==0.2.3.0,vector==0.10.0.1,xhtml==3000.2.1,zlib==0.5.4.1,aeson==0.7.0.4" > cabal.config ;;

"2012.4.0.0")
  export GHCVER=7.6.2 ;
  echo "constraints:async==2.0.1.3,cgi==3001.1.7.4,fgl==5.4.2.4,GLUT==2.1.2.1,haskell-src==1.0.1.5,html==1.0.1.2,HTTP==4000.2.5,HUnit==1.2.5.1,mtl==2.1.2,network==2.3.1.0,OpenGL==2.2.3.1,parallel==3.2.0.3,parsec==3.1.3,QuickCheck==2.5.1.1,random==1.0.1.1,regex-base==0.93.2,regex-compat==0.95.1,regex-posix==0.95.2,split==0.2.1.1,stm==2.4,syb==0.3.7,text==0.11.2.3,transformers==0.3.0.0,vector==0.10.0.1,xhtml==3000.2.1,zlib==0.5.4.0,aeson==0.7.0.4" > cabal.config ;;

"2012.2.0.0")
  export GHCVER=7.4.1 ;
  echo "constraints:cgi==3001.1.7.4,fgl==5.4.2.4,GLUT==2.1.2.1,haskell-src==1.0.1.5,html==1.0.1.2,HTTP==4000.2.3,HUnit==1.2.4.2,mtl==2.1.1,network==2.3.0.13,OpenGL==2.2.3.1,parallel==3.2.0.2,parsec==3.1.2,QuickCheck==2.4.2,random==1.0.1.1,regex-base==0.93.2,regex-compat==0.95.1,regex-posix==0.95.1,stm==2.3,syb==0.3.6.1,text==0.11.2.0,transformers==0.3.0.0,xhtml==3000.2.1,zlib==0.5.3.3,haddock==2.10.0,aeson==0.7.0.4" > cabal.config ;;

*)
  export GHCVER=unknown ;
  echo "unknown/invalid Haskell Platform requested" ;
  exit 1 ;;

esac

# Travis VMs run on 1.5 virtual cores
export NUM_CPU=2

export PATH=$HOME/.cabal/bin:/opt/ghc/$GHCVER/bin:$PATH
export CABAL=cabal-$CABALVER

# Constrain TH to the version we've got
case "$GHCVER" in
7.4*)
  export CABAL_CONSTRAINTS="--constraint=template-haskell==2.7.0.0 --constraint=haddock<2.14"
  ;;
7.6*)
  export CABAL_CONSTRAINTS="--constraint=template-haskell==2.8.0.0 --constraint=haddock<2.14"
  ;;
7.8*)
  export CABAL_CONSTRAINTS="--constraint=template-haskell==2.9.0.0"
  ;;
esac

echo "Using cabal command: $CABAL"
echo "  with constraints: $CABAL_CONSTRAINTS"
echo "PATH: $PATH"

