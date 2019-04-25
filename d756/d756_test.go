package main

import(
	"testing"
)

func TestExecute10(t *testing.T) {
	expect := 1
	actual := Execute(10)

	if expect != actual {
		t.Errorf("%d != %d", expect, actual)
	}
}

func TestExecute100(t *testing.T) {
	expect := 543
	actual := Execute(100)

	if expect != actual {
		t.Errorf("%d != %d", expect, actual)
	}
}

