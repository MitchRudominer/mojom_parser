package lexer

import (
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

	// Identifiers
	case IDENTIFIER:
		return "an identifier"

	// Keywords
	case INTERFACE:
		return "'interface'"
	case MODULE:
		return "'module'"

	// Constants
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
	CharPos int // TODO(azani) Explain what this is to rudominer
	LineNo  int // 1-based
	LinePos int // 0-based
}

// This method is used to generate user-facing strings in compilation error
// messages. This will be used for example in an error message that looks
// like the following:
// Unexpected token at line 5, column 6: '###'. Expecting '{'.
func (t Token) LocationString() string {
	return fmt.Sprintf("line %d, column %d", t.LineNo, t.LinePos)
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
	case ERROR_UNKNOWN, IDENTIFIER, STRING_LITERAL:
		return fmt.Sprintf("'%s'", token.Text)

	default:
		return token.Kind.String()
	}
}

func EOFToken() Token {
	eof := Token{Kind: EOF}
	return eof
}

type TokenStream interface {
	// Returns the next Token in the stream without advancing the cursor,
	// or returns the EOF token if the cursor is already past the end.
	PeekNext() Token

	// Advances the cursor in the stream and returns true, or else returns
	// false if the cursor is already past the end of the stream.
	ConsumeNext() bool
}

// *TokenSlice Implements TokenStream
type TokenSlice []Token

func (slice *TokenSlice) PeekNext() (token Token) {
	if len(*(slice)) == 0 {
		token = EOFToken()
		return
	}
	token = (*slice)[0]
	return
}

func (slice *TokenSlice) ConsumeNext() bool {
	if len(*(slice)) == 0 {
		return false
	}
	(*slice) = (*slice)[1:]
	return true
}

type lexer struct {
	source          string
	offset          int
	lineNo          int
	lineOffset      int
	start           int
	lineStart       int
	lineOffsetStart int
	tokens          chan Token
}

func (l *lexer) CurText() string {
	return l.source[l.start:l.offset]
}

func (l *lexer) sendToken(tokenType TokenKind) {
	l.tokens <- Token{
		Kind:    tokenType,
		Text:    l.source[l.start:l.offset],
		CharPos: l.start,
		LineNo:  l.lineStart + 1,
		LinePos: l.lineOffsetStart}
	l.startToken()
}

func (l *lexer) startToken() {
	l.start = l.offset
	l.lineStart = l.lineNo
	l.lineOffsetStart = l.lineOffset
}

func (l *lexer) Consume() {
	c, width := utf8.DecodeRuneInString(l.source[l.offset:])
	if c == '\n' {
		l.lineNo += 1
		l.lineOffset = 0
	} else {
		l.lineOffset += width
	}
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
	case isEqualsOrResponseStart(c):
		return lexEqualsOrResponse
	case isIdentifierStart(c):
		return lexIdentifier
	case isOrdinalStart(c):
		return lexOrdinal
	case isNumberStart(c):
		return lexNumber
	case isStringStart(c):
		return lexString
	}

	return lexSkip
}

func lexSkip(l *lexer) stateFn {
	l.Consume()
	l.startToken()
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

func isEqualsOrResponseStart(c rune) bool {
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

func isHexDigit(c rune) bool {
	return isDigit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
}

func isIdentifierStart(c rune) bool {
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

func isOrdinalStart(c rune) bool {
	return '@' == c
}

// Ordinals start with '@' and then some digits.
func lexOrdinal(l *lexer) stateFn {
	l.Consume()

	for isDigit(l.Peek()) {
		l.Consume()
	}

	l.sendToken(ORDINAL)

	return lexRoot
}

func isNumberStart(c rune) bool {
	return isDigit(c)
}

func lexNumber(l *lexer) stateFn {
	switch c := l.Peek(); c {
	case '0':
		return lexNumberStartWithZero
	}
	return lexDec
}

func lexDec(l *lexer) stateFn {
	for isDigit(l.Peek()) {
		l.Consume()
	}

	if isDecimalPartStart(l.Peek()) {
		return lexDecimalPart
	}

	l.sendToken(INT_CONST_DEC)

	return lexRoot
}

func lexNumberStartWithZero(l *lexer) stateFn {
	// Consume the starting 0
	l.Consume()

	switch c := l.Peek(); {
	case c == 'x' || c == 'X':
		return lexHexNumber
	case isDecimalPartStart(c):
		return lexDecimalPart
	}

	// Found a naked 0
	l.sendToken(INT_CONST_DEC)

	return lexRoot
}

func lexHexNumber(l *lexer) stateFn {
	// Consume the x or X
	l.Consume()

	for isHexDigit(l.Peek()) {
		l.Consume()
	}

	l.sendToken(INT_CONST_HEX)

	return lexRoot
}

func isDecimalPartStart(c rune) bool {
	return c == '.' || c == 'e' || c == 'E'
}

func lexDecimalPart(l *lexer) stateFn {
	// Consume '.' or 'e' or 'E'
	l.Consume()

	if c := l.Peek(); c == '+' || c == '-' {
		l.Consume()
	}

	for isDigit(l.Peek()) {
		l.Consume()
	}

	l.sendToken(FLOAT_CONST)

	return lexRoot
}

func isStringStart(c rune) bool {
	return '"' == c
}

func lexString(l *lexer) stateFn {
	// Consume opening quotes.
	l.Consume()

	for l.Peek() != '"' {
		if l.Peek() == '\\' {
			// If we see an escape character consume whatever follows blindly.
			// TODO(azani): Consider parsing escape sequences.
			l.Consume()
		}
		l.Consume()
	}

	// Consume the closing quotes
	l.Consume()

	l.sendToken(STRING_LITERAL)

	return lexRoot
}
