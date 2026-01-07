[ "$1" != "main" ] && export BETA_BUILD=TRUE
echo $1
echo "$BETA_BUILD"
cat to_build | xargs ./makeBundle.R
