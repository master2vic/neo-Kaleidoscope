#!/bin/zsh
#compelier
clang++ -g ${1}/${1}.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o ${1}/${1}