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
type Frame struct {
	fn          *object.CompiledFunction
	ip          int
	basePointer int
}

func NewFrame(fn *object.CompiledFunction, basePointer int) *Frame {
	f := &Frame{
		fn:          fn,
		ip:          -1,
		basePointer: basePointer,
	}

	return f
}

func (f *Frame) Instructions() code.Instructions {
	return f.fn.Instructions
}
