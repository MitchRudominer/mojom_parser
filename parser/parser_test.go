package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"testing"
)

// Builds a Token with the given data.
func token(text string, lineNumber, colNumber int,
	kind lexer.TokenKind) lexer.Token {
	return lexer.Token{Text: text, Kind: kind, LineNo: lineNumber, LinePos: colNumber}
}

// For testing the parser without the lexer, we want to be able to build
// streams of input tokens by hand. At the moment I am just manually
// hand-crafting streams.
func buildTestInputStream() lexer.TokenSlice {
	return lexer.TokenSlice{
		token("interface", 0, 0, lexer.INTERFACE),
		token("foo", 1, 1, lexer.IDENTIFIER),
		token("###", 5, 6, lexer.ERROR_UNKNOWN),
		token("{", 2, 2, lexer.LBRACE),
		token("}", 3, 3, lexer.RBRACE),
		token(";", 4, 4, lexer.SEMI),
		//token("###", 5, 6, lexer.ERROR_UNKNOWN),
	}
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
}
