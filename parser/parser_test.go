package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"testing"
)

func buildTestInputStream() lexer.TokenStream {
	contents := `
	[color=red, size=big]
	module foo.bar;
	import "a.b.c";
	import "d.e.f";

	interface baz{
		DoIt();
	};
	`
	return lexer.Tokenize(contents)
}

// This is not really an automated test. I am just using this as a hook
// to run the code in parser and print out some of the results. Eventually
// I will put real tests here.
func TestParseSuccess(t *testing.T) {
	inputStream := buildTestInputStream()
	parser := ParserForDebugging(inputStream)
	parser.Parse()
	fmt.Printf("Is OK: %v\n", parser.OK())
	fmt.Printf("Error message: %s\n", parser.ErrorMessage())
	fmt.Printf("Parse tree: %s\n", parser.GetParseTree())
	fmt.Printf("MojomFile:\n================\n%s",
		parser.GetMojomFile())
	fmt.Printf("MojomDescriptor:\n================\n%s",
		parser.GetMojomDescriptor())
}
