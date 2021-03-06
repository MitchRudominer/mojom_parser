package lexer

import (
	"fmt"
	"unicode/utf8"
)

func Tokenize(source string) TokenStream {
	tokens := make(chan Token)
	l := lexer{source: source, tokens: tokens}
	go l.run()
	return &TokenChan{tokenChan: tokens}
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

func (l *lexer) emitToken(tokenType TokenKind) {
	l.tokens <- Token{
		Kind:    tokenType,
		Text:    l.source[l.start:l.offset],
		CharPos: l.start,
		LineNo:  l.lineStart,
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
	if width == 0 {
		width = 1
	}

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

func (l *lexer) IsEos() bool {
	return l.offset >= len(l.source)
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
	case isNameStart(c):
		return lexName
	case isOrdinalStart(c):
		return lexOrdinal
	case isNumberStart(c):
		return lexNumber
	case isStringStart(c):
		return lexString
	case isSkippable(c):
		return lexSkip
	case isMaybeComment(c):
		return lexComment
	}

	l.Consume()
	l.emitToken(ERROR_ILLEGAL_CHAR)
	return nil
}

func isSkippable(c rune) bool {
	return c == ' ' || c == '\t' || c == '\r' || c == '\n'
}

func lexSkip(l *lexer) stateFn {
	for isSkippable(l.Peek()) {
		l.Consume()
	}
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
	l.emitToken(t)

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
		l.emitToken(RESPONSE)
	} else {
		l.emitToken(EQUALS)
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

func isNameStart(c rune) bool {
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
func lexName(l *lexer) stateFn {
	l.Consume()

	isNameRune := func(c rune) bool {
		return isAlpha(c) || isDigit(c) || c == '_'
	}

	for isNameRune(l.Peek()) {
		l.Consume()
	}

	if token, found := keywordTokens[l.CurText()]; found {
		l.emitToken(token)
	} else {
		l.emitToken(NAME)
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

	l.emitToken(ORDINAL)

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

	l.emitToken(INT_CONST_DEC)

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
	l.emitToken(INT_CONST_DEC)

	return lexRoot
}

func lexHexNumber(l *lexer) stateFn {
	// Consume the x or X
	l.Consume()

	for isHexDigit(l.Peek()) {
		l.Consume()
	}

	l.emitToken(INT_CONST_HEX)

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

	l.emitToken(FLOAT_CONST)

	return lexRoot
}

func isStringStart(c rune) bool {
	return '"' == c
}

func lexString(l *lexer) stateFn {
	// Consume opening quotes.
	l.Consume()

	for !l.IsEos() && l.Peek() != '"' && l.Peek() != '\n' {
		if l.Peek() == '\\' {
			// If we see an escape character consume whatever follows blindly.
			// TODO(azani): Consider parsing escape sequences.
			l.Consume()
		}
		l.Consume()
	}

	if l.IsEos() || l.Peek() == '\n' {
		l.emitToken(ERROR_UNTERMINATED_STRING_LITERAL)
		return nil
	}

	// Consume the closing quotes
	l.Consume()

	l.emitToken(STRING_LITERAL)

	return lexRoot
}

func isMaybeComment(c rune) bool {
	return c == '/'
}

func lexComment(l *lexer) stateFn {
	// Consume the '/'.
	l.Consume()

	switch l.Peek() {
	case '/':
		return lexSingleLineComment
	case '*':
		return lexMultiLineComment
	}

	l.emitToken(ERROR_ILLEGAL_CHAR)
	return nil
}

func lexSingleLineComment(l *lexer) stateFn {
	// Consume the '/'
	l.Consume()

	for !l.IsEos() && l.Peek() != '\n' {
		l.Consume()
	}

	l.startToken()
	return lexRoot
}

func lexMultiLineComment(l *lexer) stateFn {
	// Consume the '*'.
	l.Consume()

	for !l.IsEos() {
		if l.Peek() == '*' {
			return lexPossibleEndOfComment
		}
		l.Consume()
	}

	l.emitToken(ERROR_UNTERMINATED_COMMENT)
	return nil
}

func lexPossibleEndOfComment(l *lexer) stateFn {
	// Consume the '*'
	l.Consume()

	if l.IsEos() {
		l.emitToken(ERROR_UNTERMINATED_COMMENT)
		return nil
	}

	if l.Peek() == '/' {
		l.Consume()
		l.startToken()
		return lexRoot
	}

	return lexMultiLineComment
}
