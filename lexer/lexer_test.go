package lexer

import "testing"

func pumpTokens(tokensChan chan Token) []Token {
	tokens := []Token{}
	for token := range tokensChan {
		tokens = append(tokens, token)
	}
	return tokens
}

func TestAllSingleTokens(t *testing.T) {
	testData := []struct {
		source string
		token  TokenKind
	}{
		{"(", LPAREN},
		{")", RPAREN},
		{"[", LBRACKET},
		{"]", RBRACKET},
		{"{", LBRACE},
		{"}", RBRACE},
		{"<", LANGLE},
		{">", RANGLE},
		{";", SEMI},
		{",", COMMA},
		{".", DOT},
		{"-", MINUS},
		{"+", PLUS},
		{"&", AMP},
		{"?", QSTN},
		{"=", EQUALS},
		{"=>", RESPONSE},
		{"somet_hi3ng", IDENTIFIER},
	}

	for i := range testData {
		l := lexer{source: testData[i].source, tokens: make(chan Token)}
		go l.run()
		tokens := pumpTokens(l.tokens)

		if len(tokens) != 1 {
			t.Fatalf("Source('%v'): Expected 1 token but got %v instead: %v",
				testData[i].source, len(tokens), tokens)
		}
	}
}
