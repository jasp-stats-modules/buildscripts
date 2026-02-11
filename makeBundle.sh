[ "$1" != "main" ] && export BETA_BUILD=TRUE
[ "$2" != "-1" ] && export BUILDNUM=$2
echo "$BETA_BUILD"
echo "$BUILDNUM"
cat to_build | xargs ./makeBundle.R
