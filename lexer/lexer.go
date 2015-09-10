package lexer

import (
	"errors"
	"fmt"
)

type TokenKind int

// TokenKinds
const (
	// Errors
	ERROR_UNKNOWN TokenKind = iota
	EOF                     // Indicates the end of a stream of tokens

	// Punctuators and Separators
	LPAREN
	RPAREN
	LBRACKET
	RBRACKET
	LBRACE
	RBRACE
	LANGLE
	RANGLE
	SEMI
	COMMA
	DOT
	MINUS
	PLUS
	AMP
	QSTN
	EQUALS
	RESPONSE

	// Keywords
	IMPORT
	MODULE
	STRUCT
	UNION
	INTERFACE
	ENUM
	CONST
	TRUE
	FALSE
	DEFAULT

	// Constants
	INT_CONST_DEC
	INT_CONST_HEX
	FLOAT_CONST
	ORDINAL
	STRING_LITERAL

	// TODO(azani): Check that all tokens were implemented.
)

func (tokenKind TokenKind) String() string {
	switch tokenKind {
	case LPAREN:
		return "("
	case RPAREN:
		return ")"
	case ERROR_UNKNOWN:
		return "UNKNOWN"
	// TODO: Add the rest
	default:
		panic(fmt.Sprintf("Invalid TokenKind: %v", tokenKind))
	}
}

type Token struct {
	Kind    TokenKind
	Text    string
	CharPos int // TODO(azani) Explain what this is to rudominer
	LineNo  int // 1-based
	LinePos int // 0-based
}

func (token Token) String() string {
	// TODO Add the other fields
	return token.Kind.String()
}

func EOFToken() Token {
	eof := Token{Kind: EOF}
	return eof
}

type TokenStream interface {
	PeekNext() (Token, error)
	ConsumeNext() error
}

// Implements TokenStream
type TokenSlice []Token

func (slice *TokenSlice) PeekNext() (token Token, err error) {
	if len(*(slice)) == 0 {
		err = errors.New("EOF")
		token = EOFToken()
		return
	}
	token = (*slice)[0]
	return
}

func (slice *TokenSlice) ConsumeNext() (err error) {
	if len(*(slice)) == 0 {
		err = errors.New("EOF")
		return
	}
	(*slice) = (*slice)[1:]
	return
}
