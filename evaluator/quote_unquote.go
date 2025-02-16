package evaluator

import (
	"github.com/stefanalfbo/monkey/ast"
	"github.com/stefanalfbo/monkey/object"
)

func quote(node ast.Node) object.Object {
	return &object.Quote{Node: node}
}
