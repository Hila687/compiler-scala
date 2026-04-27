@256
D=A
@SP
M=D
@280
D=A
@ARG
M=D
@290
D=A
@LCL
M=D

// push constant 2
@2
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop argument 2
@ARG
D=M
@2
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 8
@8
D=A
@SP
A=M
M=D
@SP
M=M+1
// neg
@SP
A=M-1
M=-M
// pop local 4
@LCL
D=M
@4
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 7
@7
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop temp 0
@SP
AM=M-1
D=M
@5
M=D
// push constant 10
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
// push local 4
@LCL
D=M
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push temp 0
@5
D=M
@SP
A=M
M=D
@SP
M=M+1
// xor
@SP
AM=M-1
D=M
A=A-1
D=D&M
@R13
M=D
@SP
A=M
D=M
A=A-1
D=D|M
@R13
D=D-M
@SP
A=M-1
M=D
// pop argument 3
@ARG
D=M
@3
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 6
@6
D=A
@SP
A=M
M=D
@SP
M=M+1
// push static 3
@world.3
D=M
@SP
A=M
M=D
@SP
M=M+1
// add
@SP
AM=M-1
D=M
A=A-1
M=M+D
// pop static 3
@SP
AM=M-1
D=M
@world.3
M=D
