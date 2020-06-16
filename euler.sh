#!/usr/bin/env bash
# euler.sh

set -e

PROBLEM=$1; shift || (echo "Provide problem" && exit 1)
NAMING_SCHEME="euler"
NAME=${NAMING_SCHEME}_${PROBLEM}

pushd ~/misc/haskell/pain/pain

if [ ! -f $NAME.hs ]; then
  bazel run --experimental_ui_limit_console_output=1 @euler//stub -- b ${NAMING_SCHEME} --haskell $PROBLEM >> BUILD || exit 1
  bazel run --experimental_ui_limit_console_output=1 @euler//stub -- --haskell $PROBLEM > ${NAME}.hs || exit 1
fi

nvim $NAME.hs
status=$?
popd
exit $status
