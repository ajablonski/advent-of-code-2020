#!/bin/bash
token=$(<token.txt)
day=$1
curl "https://adventofcode.com/2020/day/$day/input" --compressed -H "Cookie: session=$token"
