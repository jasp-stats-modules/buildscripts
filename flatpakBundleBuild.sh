export LD_LIBRARY_PATH=/app/lib/x86_64-linux-gnu/:/app/lib/aarch64-linux-gnu/:/app/lib/
export C_INCLUDE_PATH=/app/include/
export CPLUS_INCLUDE_PATH=/app/include/
export PREFIX=/app/
export PKG_CONFIG_PATH=/app/lib/pkgconfig/
export DOWNLOAD_STATIC_LIBV8=1
export R_LIBS=$XDG_CACHE_HOME/../jasp_R_dev_lib/
unset R_LIBS_USER
rmdir $R_LIBS
mkdir $R_LIBS

[ "$(git rev-parse --abbrev-ref HEAD)" != "main" ] && export BETA_BUILD=TRUE

cat to_build | xargs /app/bin/Rscript makeBundle.R
