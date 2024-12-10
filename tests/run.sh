#!/usr/bin/env bash
$1 --no-color --console-width 0 --no-banner --package pg-idris --package pg-idris-tests --package indexed --package elab-util --package parser --package parser-json $2 < $3
