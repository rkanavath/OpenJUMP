#/bin/sh
##
# See oj_linux.sh for some parameters you might want to tune
# e.g. max memory, language, jre to use
##

CDIR=`dirname "$0"`

## define a default look&feel(laf)
#  "" - empty for system default
#  "javax.swing.plaf.metal" - for a cross platform ui, if problems occure
JAVA_LOOKANDFEEL=""
## some parameters to make OJ shiny ;)
JAVA_OPTS_OVERRIDE="-Xdock:name=OpenJUMP -Xdock:icon=./bin/OpenJUMP.app/Contents/Resources/appIcon.icns"

## we have only x86 compiled osx native libs, try force 32bit jre
## disabled as default, as it limits memory usage
#JAVA_OPTS_OVERRIDE="$JAVA_OPTS_OVERRIDE -d32"

## Detect RAM size in bytes
RAM_SIZE=`sysctl -a|awk '/^hw.memsize:/{print $2}'`

## add default jre location to end of PATH env var
## oracle jre install does not do that, who knows why?
JRE_BIN="/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java"
[ -z "$JAVA_HOME" ] && [ -x "$JRE_BIN" ] && \
JAVA_HOME=$(echo "$JRE_BIN" | awk '{sub(/\/[^\/]+\/[^\/]+$/,"",$0);print $0}')

## run the real magic now
. "$CDIR/oj_linux.sh" "$@"
    