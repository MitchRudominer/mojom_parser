package parser

import (
	"fmt"
	"testing"
)

func buildTestFileContents() string {
	contents := `
	[color=red, size=big]
	module foo.bar;
	import "a.b.c";
	import "d.e.f";

	interface baz{
		DoIt();
	};
	`
	return contents
}

// This is not really an automated test. I am just using this as a hook
// to run the code in parser and print out some of the results. Eventually
// I will put real tests here.
func TestParseSuccess(t *testing.T) {
	fileContents := buildTestFileContents()
	parser := ParserForDebugging(fileContents)
	parser.Parse()
	fmt.Printf("Is OK: %v\n", parser.OK())
	fmt.Printf("Error message: %s\n", parser.ErrorMessage())
	fmt.Printf("Parse tree: %s\n", parser.GetParseTree())
	fmt.Printf("MojomFile:\n================\n%s",
		parser.GetMojomFile())
	fmt.Printf("MojomDescriptor:\n================\n%s",
		parser.GetMojomDescriptor())
}
