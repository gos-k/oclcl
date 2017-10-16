#!/bin/bash

[ -d pocl ] && exit 0

git clone --depth=1 https://github.com/pocl/pocl.git $HOME/pocl

cd $HOME/pocl
mkdir build
cd build
cmake ..

make && sudo make install

