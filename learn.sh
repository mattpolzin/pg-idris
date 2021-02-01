build_out="$(idris2 --build pg-idris.ipkg)"
status="$?"
clear
if [[ "$status" == "0" ]]; then
  echo "All good."
else
  echo "$build_out"
fi
