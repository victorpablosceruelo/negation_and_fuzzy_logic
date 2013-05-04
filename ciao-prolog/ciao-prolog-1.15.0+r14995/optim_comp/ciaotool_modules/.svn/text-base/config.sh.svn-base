# ---------------------------------------------------------------------------
# Configuration variables

cache_dir=${HOME}/.ciao-cache
# Set the directory where executables goes
bin_dir=${cache_dir}/bin

tmpcomp_dir=${cache_dir}/tmpcomp

# ---------------------------------------------------------------------------
# Custom procedures and post-initialization of variables computed from
# the configuration variables

ensure_bin_dir() {
    mkdir -p "${bin_dir}"
}

# ---------------------------------------------------------------------------
# Regression data

regression_name="CiaoRegressionData"
regression_repo="ssh://clip.dia.fi.upm.es/home/clip/Systems/${regression_name}"
regression_base="${ocroot}/.."
regression_dir="${regression_base}/${regression_name}"

ensure_regression_dir() {
    if [ -r "${regression_dir}/.git" ]; then
	true
    else
	cat <<EOF
The regression data directory \`${regression_dir}' is not found or
does not seem to be cloned.

Please, clone it by typing:

$ cd ${regression_base}
$ git clone ${regression_repo}
EOF
	exit -1
    fi
}

