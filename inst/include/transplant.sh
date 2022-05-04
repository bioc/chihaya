#!/bin/bash

set -e
set -u

# Transplant the code.
if [ -e source ]
then
    (cd source && git pull)
else
    git clone https://github.com/LTLA/chihaya source
fi

rm -rf chihaya
mkdir chihaya
cp source/include/chihaya/* chihaya
