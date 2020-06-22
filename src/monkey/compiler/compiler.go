package compiler

import (
	"monkey/ast"
	"monkey/code"
	"monkey/object"
)

// Compiler - instructions will hold the generated
// bytecode and constants is a slice that serves as
// the constant pool
type Compiler struct {
	instructions code.Instructions
	constants    []object.Object
}

func New() *Compiler {
	return &Compiler{
		instructions: code.Instructions{},
		constants:    []object.Object{},
	}
}

func (c *Compiler) Compile(node ast.Node) error {
	return nil
}

func (c *Compiler) Bytecode() *Bytecode {
	return &Bytecode{
		Instructions: c.instructions,
		Constants:    c.constants,
	}
}

// Bytecode contains the Instructions the compiler
// generated and the Constants the compiler evaluated
// This is what will be passed to the VM
type Bytecode struct {
	Instructions code.Instructions
	Constants    []object.Object
}
