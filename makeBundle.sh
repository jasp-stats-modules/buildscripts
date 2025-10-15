[ "$(git rev-parse --abbrev-ref HEAD)" != "main" ] && export BETA_BUILD=TRUE
echo "$BETA_BUILD"
cat to_build | xargs /app/bin/Rscript makeBundle.R
