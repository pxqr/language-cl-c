# Synopsis

EDSL for OpenCL C language deep embedded in Haskell. 

# Description

This package might be used as backend for runtime generation software. Note that typed representation of AST force to use only type-preserving transformation. The main goals:

* Typesafety through HOAS. Any edsl code passed typecheck by Haskell compiler should pass typecheck and syntax check by OpenCL C compiler. 
* Independent from a particular FFI binding to OpenCL. Therefore the package defines all hardware-side datatypes but doesn't defines API-side datatypes. Consequently, mapping from first to last does not defined by the package  and should be done by third side.
* Human-readable. Embedded code could be easily and safely writen by hand, it's useful for defining skeletons. Code could be generated at run time in typesafe fashion as well.
* NOT IMPLEMENTED YET. Optional caching. Compiled code are cached and stored in hash map for future reuse. 

# Features

Currently implemented:

* Strict subset of CL C language. 
* The strict subset expanded through name mangling:
    1. Funargs. At "generation time". 
    2. Parametric polymorphism. 
 
  This and a bunch of typeclasses is useful for defining generic skeletons and it's makes front-end more simple.

Not implemented yet:

* rvalue/lvalue at type level.
* const memory qualifier.
* global, local memory spaces.
* a few of builtin functions and types.

# Outline

# Exsamples

STUB
