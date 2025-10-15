[ "$(git rev-parse --abbrev-ref HEAD)" != "main" ] && export BETA_BUILD=TRUE
cat to_build | xargs /app/bin/Rscript makeBundle.R
