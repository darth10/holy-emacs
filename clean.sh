#!/bin/bash

find . -name '*.elc' | xargs rm -f
find . -name '*.el~' | xargs rm -f
