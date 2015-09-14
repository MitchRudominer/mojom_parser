package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"testing"
)

type P struct {
	kind lexer.TokenKind
	text string
}

func buildStream(tokens []interface{}) lexer.TokenSlice {
	slice := make(lexer.TokenSlice, len(tokens))
	for i, x := range tokens {
		text := ""
		var kind lexer.TokenKind
		if k, ok := x.(lexer.TokenKind); ok {
			kind = k
		} else {
			p := x.(P)
			text = p.text
			kind = p.kind
		}
		slice[i] = lexer.Token{Text: text, Kind: kind, LineNo: i, LinePos: i}
	}
	return slice
}

// For testing the parser without the lexer, we want to be able to build
// streams of input tokens by hand. At the moment I am just manually
// hand-crafting streams.
func buildTestInputStream() lexer.TokenSlice {
	return buildStream(
		[]interface{}{
			lexer.LBRACKET,
			P{lexer.IDENTIFIER, "color"},
			//lexer.EQUALS,
			P{lexer.IDENTIFIER, "red"},
			lexer.COMMA,
			P{lexer.IDENTIFIER, "size"},
			lexer.EQUALS,
			P{lexer.IDENTIFIER, "big"},
			lexer.RBRACKET,
			lexer.MODULE,
			P{lexer.IDENTIFIER, "foo.bar"},
			lexer.SEMI,
			lexer.IMPORT,
			P{lexer.STRING_LITERAL, "a.b.c"},
			lexer.SEMI,
			lexer.IMPORT,
			P{lexer.STRING_LITERAL, "d.e.f"},
			lexer.SEMI,
			lexer.INTERFACE,
			P{lexer.IDENTIFIER, "baz"},
			lexer.LBRACE,
			lexer.RBRACE,
			lexer.SEMI,
		})
}

// This is not really an automated test. I am just using this as a hook
// to run the code in parser and print out some of the results. Eventually
// I will put real tests here.
func TestParseSuccess(t *testing.T) {
	inputStream := buildTestInputStream()
	parser := ParserForDebugging(&inputStream)
	parser.Parse()
	fmt.Printf("Is OK: %v\n", parser.OK())
	fmt.Printf("Error message: %s\n", parser.ErrorMessage())
	fmt.Printf("Parse tree: %s\n", parser.GetParseTree())
	fmt.Printf("MojomFile:\n================\n%s",
		parser.GetMojomFile())
	fmt.Printf("MojomDescriptor:\n================\n%s",
		parser.GetMojomDescriptor())
}
