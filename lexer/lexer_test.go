package lexer

import "testing"

func checkEq(t *testing.T, expected, actual interface{}) {
	if expected != actual {
		t.Fatalf("Failed check: Expected (%v), Actual (%v)", expected, actual)
	}
}

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
		{"import", IMPORT},
		{"module", MODULE},
		{"struct", STRUCT},
		{"union", UNION},
		{"interface", INTERFACE},
		{"enum", ENUM},
		{"const", CONST},
		{"true", TRUE},
		{"false", FALSE},
		{"default", DEFAULT},
		{"@10", ORDINAL},
		{"10", INT_CONST_DEC},
		{"0", INT_CONST_DEC},
		{"0xA10", INT_CONST_HEX},
		{"0xa10", INT_CONST_HEX},
		{"0XA10", INT_CONST_HEX},
		{"0Xa10", INT_CONST_HEX},
		{"10.5", FLOAT_CONST},
		{"10e5", FLOAT_CONST},
		{"0.5", FLOAT_CONST},
		{"0e5", FLOAT_CONST},
		{"10e+5", FLOAT_CONST},
		{"10e-5", FLOAT_CONST},
		{"\"hello world\"", STRING_LITERAL},
		{"\"hello \\\"real\\\" world\"", STRING_LITERAL},
	}

	for i := range testData {
		l := lexer{source: testData[i].source, tokens: make(chan Token)}
		go l.run()
		tokens := pumpTokens(l.tokens)

		if len(tokens) != 1 {
			t.Fatalf("Source('%v'): Expected 1 token but got %v instead: %v",
				testData[i].source, len(tokens), tokens)
		}

		checkEq(t, testData[i].source, tokens[0].Text)
		checkEq(t, testData[i].token, tokens[0].Kind)
	}
}

func TestTokenPosition(t *testing.T) {
	source := "  \n  ."
	l := lexer{source: source, tokens: make(chan Token)}
	go l.run()
	tokens := pumpTokens(l.tokens)
	token := tokens[0]

	checkEq(t, 5, token.CharPos)
	checkEq(t, 2, token.LineNo)
	checkEq(t, 2, token.LinePos)
}

func TestSkipSkippable(t *testing.T) {
	source := "  \t  \r \n  ."
	l := lexer{source: source, tokens: make(chan Token)}
	go l.run()
	tokens := pumpTokens(l.tokens)

	checkEq(t, DOT, tokens[0].Kind)
}
