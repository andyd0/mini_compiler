package vm

import (
	"monkey/code"
	"monkey/object"
)

// ip is a pointer in this frame for this function.
// basePointer is used to keep track of the stack
// pointer's value before executing a function and
// restore it to this value after excuting. this is
// temporary storage that lives as long as a function
// call so makes sense to use the frame. base pointer
// is usually the name given to this pointer that points
// to the bottom of the stack of the current call frame.
// it is also called frame pointer
// first line was a reference to a compiled function but
// changed to closure to provide support for closures as
// every function is being treated as a closure.
type Frame struct {
	cl          *object.Closure
	ip          int
	basePointer int
}

func NewFrame(cl *object.Closure, basePointer int) *Frame {
	f := &Frame{
		cl:          cl,
		ip:          -1,
		basePointer: basePointer,
	}

	return f
}

// Because of the indirection, instead of going through
// fn that holds an *object.CompiledFunction now has a cl
// field pointing to an *object.Closure. To get the instructions
// go through cl field first and then through the Fn the
// closure is wrapping.
func (f *Frame) Instructions() code.Instructions {
	return f.cl.Fn.Instructions
}
