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
	RIG_LIST_OUTPUT=$(rig list)
	# `rig default` wants the installation name as shown by `rig list`. That
	# name is the full version on Linux/Windows, but on macOS it is
	# minor-version + architecture (e.g. 4.5-x86_64), so look up the name on
	# the line that reports exactly our requested R version.
	RIG_NAME=$(printf '%s\n' "$RIG_LIST_OUTPUT" | awk -v target="(R $4)" 'index($0, target) { for (i = 1; i <= NF; i++) if ($i != "*") { print $i; exit } }')
	if [ -z "$RIG_NAME" ]; then
		echo "ERROR: R $4 is not installed on this worker (no entry in 'rig list'), install it once with 'rig add $4'" >&2
		exit 1
	fi
	# Switching the default writes to system locations and therefore needs
	# admin rights, which the buildbot worker usually does not have. So skip
	# the switch when the requested version already is the default (rig list
	# marks that line with '*').
	CURRENT_R_VERSION=$(printf '%s\n' "$RIG_LIST_OUTPUT" | awk 'substr($0, 1, 1) == "*" { for (i = 1; i <= NF; i++) if ($i == "(R") { v = $(i+1); sub(/\)$/, "", v); print v; exit } }')
	if [ "$CURRENT_R_VERSION" != "$4" ]; then
		echo "NOTE: switching to R $4, which rig knows as '$RIG_NAME'"
		SWITCHED=0
		rig default "$RIG_NAME" && SWITCHED=1
		if [ "$SWITCHED" -eq 0 ] && command -v sudo >/dev/null 2>&1; then
			echo "NOTE: retrying 'rig default $RIG_NAME' with passwordless sudo"
			sudo -n rig default "$RIG_NAME" && SWITCHED=1
		fi
		if [ "$SWITCHED" -eq 0 ]; then
			echo "ERROR: 'rig default $RIG_NAME' failed; switching the default R needs admin rights" >&2
			echo "HINT: run 'sudo rig default $RIG_NAME' once on this worker, or allow the buildbot user to run rig without a password in sudoers: <buildbot-user> ALL=(ALL) NOPASSWD: $(command -v rig)" >&2
			exit 1
		fi
	else
		echo "NOTE: R $4 is already the default on this worker"
	fi
	# Make sure the requested version is really the one selected now.
	ACTUAL_R_VERSION=$(Rscript -e 'cat(paste(R.version$major, R.version$minor, sep="."))' 2>/dev/null)
	if [ "$ACTUAL_R_VERSION" != "$4" ]; then
		echo "ERROR: rig default resolved to R ${ACTUAL_R_VERSION:-<none>} but R $4 was requested; install R $4 on this worker with 'rig add $4'" >&2
		exit 1
	fi
fi

echo "$2"
echo "$BETA_BUILD"
echo "$BUILDNUM"
echo "$COMPAT_VERSION"
echo "$4"
cat to_build | xargs ./makeBundle.R
