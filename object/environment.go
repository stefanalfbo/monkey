package object

type Environment struct {
	store map[string]Object
	outer *Environment
}

func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s, outer: nil}
}

func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer

	return env
}

func (e *Environment) Get(name string) (Object, bool) {
	object, ok := e.store[name]

	if !ok && e.outer != nil {
		object, ok = e.outer.Get(name)
	}

	return object, ok
}

func (e *Environment) Set(name string, value Object) Object {
	e.store[name] = value

	return value
}
