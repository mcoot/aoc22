#!/usr/bin/env bash

PARAM_DAY="$1"
PARAM_YEAR="${2:-2022}"

SESSION_TOKEN=$(cat ./aoc.sessiontoken)

echo "Downloading Day $PARAM_DAY (Year ${PARAM_YEAR})"
curl --cookie "session=$SESSION_TOKEN" "https://adventofcode.com/$PARAM_YEAR/day/$PARAM_DAY/input" > "./data/input/day$PARAM_DAY.in"
echo "Done!"