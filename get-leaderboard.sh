#!/bin/bash
token=$(<token.txt)
curl 'https://adventofcode.com/2020/leaderboard/private/view/387323.json' --compressed -H "Cookie: session=$token"
