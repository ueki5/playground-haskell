module CodeGen (codegen) where

import Compiler (Instr (..))

codegen :: [Instr] -> String
codegen instrs =
  unlines $
    prologue
      ++ concatMap genInstr instrs
      ++ epilogue

prologue :: [String]
prologue =
  [ "    .section .text",
    "    .globl main",
    "main:",
    "    pushq %rbp",
    "    movq  %rsp, %rbp"
  ]

epilogue :: [String]
epilogue =
  [ "    popq  %rsi",
    "    leaq  fmt(%rip), %rdi",
    "    xorl  %eax, %eax",
    "    call  printf",
    "    xorl  %eax, %eax",
    "    popq  %rbp",
    "    ret",
    ".Ldiv_zero_error:",
    "    leaq  errmsg(%rip), %rdi",
    "    call  puts",
    "    movl  $1, %edi",
    "    call  exit",
    "    .section .rodata",
    "fmt:",
    "    .string \"%d\\n\"",
    "errmsg:",
    "    .string \"division by zero\"",
    "    .section .note.GNU-stack,\"\",@progbits"
  ]

genInstr :: Instr -> [String]
genInstr (Push n) =
  [ "    pushq $" ++ show n
  ]
genInstr IAdd =
  [ "    popq  %rax",
    "    popq  %rbx",
    "    addq  %rbx, %rax",
    "    pushq %rax"
  ]
genInstr ISub =
  [ "    popq  %rax",
    "    popq  %rbx",
    "    subq  %rax, %rbx",
    "    pushq %rbx"
  ]
genInstr IMul =
  [ "    popq  %rax",
    "    popq  %rbx",
    "    imulq %rbx, %rax",
    "    pushq %rax"
  ]
genInstr IDiv =
  [ "    popq  %rcx",
    "    cmpq  $0, %rcx",
    "    je    .Ldiv_zero_error",
    "    popq  %rax",
    "    cqto",
    "    idivq %rcx",
    "    pushq %rax"
  ]
genInstr INeg =
  [ "    popq  %rax",
    "    negq  %rax",
    "    pushq %rax"
  ]
