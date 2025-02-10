package object

type Environment struct {
	store map[string]Object
}

func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s}
}

func (e *Environment) Get(name string) (Object, bool) {
	object, ok := e.store[name]

	return object, ok
}

func (e *Environment) Set(name string, value Object) Object {
	e.store[name] = value

	return value
}
