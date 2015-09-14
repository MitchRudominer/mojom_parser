package lexer

import (
	"errors"
	"fmt"
	"unicode/utf8"
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

	// Identifiers
	IDENTIFIER

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

type lexer struct {
	source     string
	offset     int
	start      int
	lineNo     int
	lineOffset int
	tokens     chan Token
}

func (l *lexer) CurText() string {
	return l.source[l.start:l.offset]
}

func (l *lexer) sendToken(tokenType TokenKind) {
	l.tokens <- Token{
		Kind:    tokenType,
		Text:    l.source[l.start:l.offset],
		CharPos: l.start,
		LineNo:  l.lineNo,
		LinePos: l.lineOffset}
	l.start = l.offset
}

func (l *lexer) Consume() {
	_, width := utf8.DecodeRuneInString(l.source[l.offset:])
	l.offset += width
}

func (l lexer) String() string {
	return fmt.Sprintf("{offset: %v, Peek: '%v'}", l.offset, l.Peek())
}

func (l *lexer) Peek() rune {
	char, _ := utf8.DecodeRuneInString(l.source[l.offset:])
	return char
}

func (l *lexer) run() {
	for state := lexRoot; state != nil; {
		state = state(l)
	}
	close(l.tokens)
}

type stateFn func(*lexer) stateFn

func lexRoot(l *lexer) stateFn {
	if l.offset >= len(l.source) {
		return nil
	}

	switch c := l.Peek(); {
	case isSingleCharTokens(c):
		return lexSingleCharTokens
	case isEqualsOrResponse(c):
		return lexEqualsOrResponse
	case isIdentifier(c):
		return lexIdentifier
	}

	return lexSkip
}

func lexSkip(l *lexer) stateFn {
	l.Consume()
	l.start = l.offset
	return lexRoot
}

var singleCharTokens = map[rune]TokenKind{
	'(': LPAREN,
	')': RPAREN,
	'[': LBRACKET,
	']': RBRACKET,
	'{': LBRACE,
	'}': RBRACE,
	'<': LANGLE,
	'>': RANGLE,
	';': SEMI,
	',': COMMA,
	'.': DOT,
	'-': MINUS,
	'+': PLUS,
	'&': AMP,
	'?': QSTN,
}

func isSingleCharTokens(c rune) bool {
	_, ok := singleCharTokens[c]
	return ok
}

func lexSingleCharTokens(l *lexer) stateFn {
	c := l.Peek()
	l.Consume()
	t, _ := singleCharTokens[c]
	l.sendToken(t)

	return lexRoot
}

func isEqualsOrResponse(c rune) bool {
	return c == '='
}

// Finds '=' or '=>'.
func lexEqualsOrResponse(l *lexer) stateFn {
	l.Consume()

	if l.Peek() == '>' {
		l.Consume()
		l.sendToken(RESPONSE)
	} else {
		l.sendToken(EQUALS)
	}

	return lexRoot
}

// Non-localized versions of isalpha.
func isAlpha(c rune) bool {
	return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
}

// Non-localized versions of isnum.
func isDigit(c rune) bool {
	return ('0' <= c && c <= '9')
}

func isIdentifier(c rune) bool {
	return isAlpha(c) || c == '_'
}

var keywordTokens = map[string]TokenKind{
	"import":    IMPORT,
	"module":    MODULE,
	"struct":    STRUCT,
	"union":     UNION,
	"interface": INTERFACE,
	"enum":      ENUM,
	"const":     CONST,
	"true":      TRUE,
	"false":     FALSE,
	"default":   DEFAULT,
}

// valid C identifiers (K&R2: A.2.3)
func lexIdentifier(l *lexer) stateFn {
	l.Consume()

	isIdRune := func(c rune) bool {
		return isAlpha(c) || isDigit(c) || c == '_'
	}

	for isIdRune(l.Peek()) {
		l.Consume()
	}

	if token, found := keywordTokens[l.CurText()]; found {
		l.sendToken(token)
	} else {
		l.sendToken(IDENTIFIER)
	}

	return lexRoot
}
