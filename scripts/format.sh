#!/usr/bin/env bash

find src test -name "*.hs" | xargs stack exec ormolu -- -m inplace