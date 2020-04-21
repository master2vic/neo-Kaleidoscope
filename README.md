# neo-Kaleidoscope

个人学习llvm使用,使用c++重写官方提供的ocamlbuild实现的Kaleidoscope。同时也参考了一些llvm中文社区。

我之前学SICP的时候接触过Lisp语言和一些变种Racket。所以对这个Kaleidoscope比较容易。

## 运行说明

### 环境需求

需要llvm+clang

### 构建指令

~~~shell
mkdir bin # 创建文件夹存储二进制文件
chmod +x ./build.sh #添加脚本
./build.sh <targetname>
~~~

注意这里的`<targetname>`不是`./src/step1_lexer_ast_parse.cpp`

而是`step1_lexer_ast_parse`

### 测试说明

~~~shell
chmod +x ./test.sh
./test.sh <testfile> <targetname>
~~~

同理，这里`testfile`也不需要加地址，直接给名字就行

## 进度

### 参考部分（中文社区只有前4个，官网提供全部章届）

- [X] lexer
- [X] Parser and AST
- [X] Code generation to LLVM IR
- [x] Control Flow
- [ ] Adding JIT and Optimizer Support
- [ ] User-defined Operators
- [ ] Mutable Variables

<<<<<<< HEAD
### 个人期望部分 (暂定，不一定会增加)
=======
### 实现不同之处

1. 函数的入口不同
    
    为了统一性，我特别的增加了一个token的状态：`tok-init`用于作为词法分析的入口。
2. 增加debug模式，输出调试信息

### 个人期望部分
>>>>>>> 65de8e01e3b0ed0f313def5639daa2b94f15e468

- [ ] Object Oriented
- [x] recursion
- [ ] lambda
- [ ] reflection

## 参考

- [llvm tutorial](https://llvm.org/docs/tutorial/index.html)
- [知乎文章](https://zhuanlan.zhihu.com/p/122522485)

## 参与人员

- 石渊友,湖南大学（2017~2021 计算机本科） (Shi Yuanyou, Hunan University 2017 ~2021 CS undergraduate student) 


