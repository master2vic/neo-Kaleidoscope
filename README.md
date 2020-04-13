# neo-Kaleidoscope

个人学习llvm使用,使用c++重写官方提供的ocamlbuild实现的Kaleidoscope。同时也参考了一些llvm中文社区。

我之前学SICP的时候接触过Lisp语言和一些变种Racket。所以对这个Kaleidoscope比较容易。

## 进度

### 参考部分（中文社区只有前4个，官网提供全部章届）

- [X] lexer
- [X] Parser and AST
- [X] Code generation to LLVM IR
- [ ] Adding JIT and Optimizer Support
- [ ] Control Flow
- [ ] User-defined Operators
- [ ] Mutable Variables

### 实现不同之处

1. 函数的入口不同
    
    为了统一性，我特别的增加了一个token的状态：`tok-init`用于作为词法分析的入口。
2. 剔除c的影子，保留c++
3. 增加debug模式，输出调试信息

### 个人期望部分

- [ ] Object Oriented
- [ ] recursion
- [ ] lambda
- [ ] reflection

## 参考

- [llvm tutorial](https://llvm.org/docs/tutorial/index.html)
- [llvm 中文教程](https://llvm.zcopy.site/tutorial/langimpl)
- [知乎文章](https://zhuanlan.zhihu.com/p/122522485)

## 参与人员

- 石渊友,湖南大学（2017~2021 计算机本科） (Shi Yuanyou, Hunan University 2017 ~2021 CS undergraduate student) 


