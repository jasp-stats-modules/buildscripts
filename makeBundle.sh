[ "$1" != "main" ] && export BETA_BUILD=TRUE
[ "$2" != "-1" ] && export BUILDNUM=$2

# $3 (optional): JASP compatibility version the modules are built for
# (buildforJaspVersion in makeBundle.R). When empty, makeBundle.R falls
# back to jasp-desktop's development version.txt.
[ -n "$3" ] && export COMPAT_VERSION=$3

# $4 (optional): R version to build with, selected via rig. The version must
# already be installed on the worker (`rig add` needs sudo/a password, so
# versions are installed once manually during provisioning, not by builds).
# When empty, the R currently default on the worker is used.
if [ -n "$4" ]; then
	if ! command -v rig >/dev/null 2>&1; then
		echo "ERROR: rig is not installed on this worker, cannot switch to R $4" >&2
		exit 1
	fi
	if ! rig default "$4"; then
		echo "ERROR: R $4 is not installed on this worker, install it once with 'rig add $4'" >&2
		exit 1
	fi
fi

echo "$2"
echo "$BETA_BUILD"
echo "$BUILDNUM"
echo "$COMPAT_VERSION"
echo "$4"
cat to_build | xargs ./makeBundle.R
