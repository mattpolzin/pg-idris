#!/usr/bin/env sh

if [[ "$1" == "" ]]; then
  ${SHELL:-bash} $(pwd)/watch_and_learn.sh watch
  exit 0
fi

function watch() {
  clear
  echo "waiting..."
  fswatch -E --latency 1 --exclude='(.+\.idr~)|(.+\.swp.*)' -ro ./ | xargs -n1 ./watch_and_learn.sh
}

function learn() {
  build_out="$(idris2 --build *.ipkg)"
  status="$?"
  clear
  if [[ "$status" == "0" ]]; then
    echo "All good."
  else
    echo "$build_out"
  fi
}

if [[ "$(which fswatch)" == "" ]]; then
  echo "fswatch not installed."
  echo ""
  exit 1
fi

if [[ "$1" == 'watch' ]]; then
  watch
else
  learn
fi

