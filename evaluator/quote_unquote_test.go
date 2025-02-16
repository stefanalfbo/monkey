package evaluator

import (
	"testing"

	"github.com/stefanalfbo/monkey/object"
)

func TestQuote(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{`quote(5)`, `5`},
		{`quote(5 + 8)`, `(5 + 8)`},
		{`quote(foobar)`, `foobar`},
		{`quote(foobar + barfoo)`, `(foobar + barfoo)`},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		quote, ok := evaluated.(*object.Quote)

		if !ok {
			t.Fatalf("object is not Quote. got=%T (%+v)", evaluated, evaluated)
		}

		if quote.Node == nil {
			t.Fatalf("quote.Node is nil")
		}

		if quote.Node.String() != tt.expected {
			t.Fatalf("quote.Node.String() not %s. got=%s", tt.expected, quote.Node.String())
		}
	}
}
