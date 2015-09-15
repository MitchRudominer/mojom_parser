// Copyright 2015 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

package lexer

import (
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

	// Names
	NAME

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

// This method is used to generate user-facing strings in compilation error
// messages. For example for LBRACE we produce the string "'{'". Notice the
// single-quotes. This will be used for example in an error message that looks
// like the following:
// Unexpected token at line 5, column 6: '###'. Expecting '{'.
func (tokenKind TokenKind) String() string {
	switch tokenKind {
	// Errors
	case ERROR_UNKNOWN:
		return "UNKNOWN"

	// Punctuators and Separators
	case LPAREN:
		return "'('"
	case RPAREN:
		return "')'"
	case LBRACKET:
		return "'['"
	case RBRACKET:
		return "']'"
	case LANGLE:
		return "'<'"
	case RANGLE:
		return "'>'"
	case LBRACE:
		return "'{'"
	case RBRACE:
		return "'}'"
	case SEMI:
		return "';'"
	case COMMA:
		return "','"
	case EQUALS:
		return "'='"

	// Identifiers
	case NAME:
		return "a name"

	// Keywords
	case IMPORT:
		return "import"
	case INTERFACE:
		return "'interface'"
	case MODULE:
		return "'module'"

	// Constants
	case ORDINAL:
		return "an ordinal"
	case STRING_LITERAL:
		return "a string literal"

	// TODO: Add the rest
	default:
		// Note(rudominer) Be very careful not to do what I tried to do in
		// the commented-out line below. This causes an infinite recursion
		// because Sprintf invokes TokenKind.toString() when a '%v' is used.
		// It works fine to use a '%d' instead in this case.
		//panic(fmt.Sprintf("Invalid TokenKind: %v", tokenKind))
		return fmt.Sprintf("%d", tokenKind)
	}
}

type Token struct {
	Kind    TokenKind
	Text    string
	CharPos int // Position in the source string.
	LineNo  int // First line is 0.
	LinePos int // Position in the line. First char in line is 0.
}

// This method is used to generate user-facing strings in compilation error
// messages. This will be used for example in an error message that looks
// like the following:
// Unexpected token at line 5, column 6: '###'. Expecting '{'.
func (t Token) LocationString() string {
	return fmt.Sprintf("line %d, column %d", t.LineNo+1, t.LinePos+1)
}

// Is this the EOF token that represent the end of the token stream?
func (t Token) EOF() bool {
	return t.Kind == EOF
}

// This method is used to generate user-facing strings in compilation error
// messages. For many token kinds the TokenKind.String() method will produce
// good results for representing the token. But for other TokenKinds we will
// want to include some information besides a representation of the kind.
// For example for an ERROR_UNKNOWN kind we wnat to show the text.
// This will be used for example in an error message that looks
// like the following:
// Unexpected token at line 5, column 6: '###'. Expecting '{'.
func (token Token) String() string {
	switch token.Kind {
	case ERROR_UNKNOWN, NAME, STRING_LITERAL:
		return fmt.Sprintf("'%s'", token.Text)

	default:
		return token.Kind.String()
	}
}
