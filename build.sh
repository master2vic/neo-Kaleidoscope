#!/bin/zsh
#compelier
clang++ -g ./src/${1}.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o ./bin/${1}