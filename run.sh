#!/bin/bash

sbcl --eval '(ql:quickload "fiveam")' --eval '(load "testing.lsp")' --eval '(exit)'