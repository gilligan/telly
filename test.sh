#!/usr/bin/env nix-shell
#!nix-shell -i bash -p jq

echo "Building json output"

rm result
nix build
./result/bin/telly ./tv.html | jq "." > actual.json

echo "Indenting expected output"
cat ./sample.json | jq "." > expected.json

echo "Comparing expected with actual .."
diff actual.json expected.json

if [[ $? -ne 0 ]]; then
    echo "oops, guess they don't match"
    exit 1
fi

echo "Looks like we've got a match"
exit 0
