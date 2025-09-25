#!/usr/bin/env bash
$1 --no-color --console-width 0 --no-banner --package pg-idris --package pg-idris-tests --package indexed --package elab-util --package parser --package parser-json --package bytestring --package algebra --package ref1 --package array --package ilex-core --package linear $2 < $3
