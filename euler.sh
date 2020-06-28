#!/usr/bin/env bash
# euler.sh

set -e

FLAG="--edit"
if [ $# -gt 1 ]; then
  FLAG=$1; shift
fi

PROBLEM=$1; shift || (echo "Provide problem" && exit 1)
NAMING_SCHEME="euler"
NAME=${NAMING_SCHEME}_${PROBLEM}

pushd ~/misc/haskell/pain/pain

if [ $FLAG == "--examine" ]; then
  bazel run --experimental_ui_limit_console_output=1 @euler//examine $PROBLEM
  status=$?
elif [ $FLAG == "--edit" ]; then
  if [ ! -f $NAME.hs ]; then
    bazel run --experimental_ui_limit_console_output=1 @euler//stub -- b ${NAMING_SCHEME} --haskell $PROBLEM >> BUILD || exit 1
    bazel run --experimental_ui_limit_console_output=1 @euler//stub -- --haskell $PROBLEM > ${NAME}.hs || exit 1
  fi
  nvim $NAME.hs
  status=$?
elif [ $FLAG == "--test" ] && [ $PROBLEM == "all" ]; then
  bazel test :all $@
  status=$?
else
  if [ ! -f $NAME.hs ]; then
    echo "Expect file does not exist."
    popd
    exit 1
  fi
  if [ $FLAG == "--test" ]; then
    bazel test :euler_${PROBLEM}_test $@
    status=$?
  elif [ $FLAG == "--repl" ]; then
    bazel run :euler_bin${PROBLEM}@repl $@
    status=$?
  else
    echo "Unknown flag";
    popd
    exit 1
  fi
fi

popd
exit $status
