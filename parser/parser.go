package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"github.com/rudominer/mojom_parser/mojom"
	"strings"
)

// The code in this file implements a recursive-descent, predictive parser
// for the context-free grammar listed below.
//
// The grammar is similar to the grammar in the document "Mojom Language
// Specification", but it has been modified in order to make it LL(1). This
// is necessary in order to be able to use it to do predictive top-down
// parsing. (See Section 4.4.3 of "Compilers: Principles, Techniques and Tools"
// 2nd Edition, by Aho et al.")
//
// In particular we must take special care of how we handle Mojom attributes.
// Our start symbol is ATTR_MOJOM_FILE which means a Mojom file that may start
// with attributes. The symbol MOJOM_FILE will refer to a Mojom file that
// does not start with attributes. Similarly an ATR_MOJOM_DECL is a Mojom
// declaration that may start with attributes whereas as MOJOM_DECL is a Mojom
// declaration that does not start with attributes.
//
// Key:
// Upper case means non-terminals.
// Lower case means terminals and refers to the TokenKind enum in lexer.go
// Vertical bar | means alternatives.
// Braces {} means zero or more.
// Brackets [] means zero or one.
//
// ATTR_MOJOM_FILE  -> ATTRIBUTES MOJOM_FILE | MOJOM_FILE
// MOJOM_FILE       -> MODULE_DECL {IMPORT_STMNT} {MOJOM_DECL}
// MOJOM_FILE       -> IMPORT_STMNT {IMPORT_STMNT} {MOJOM_DECL}
// MOJOM_FILE       -> MOJOM_DECL {ATTR_MOJOM_DECL}
// ATTR_MOJOM_DECL  -> ATTRIBUTES MOJOM_DECL | MOJOM_DECL
// MOJOM_DECL       -> INTRFC_DECL | STRUCT_DECL | UNION_DECL | ENUM_DECL | CONSTANT_DECL
// ATTRIBUTES       -> lbracket ATTR_ASSIGNMENT { comma, ATTR_ASSIGNMENT}
// ATTR_ASSIGNMENT  -> NAME equals NAME | NAME equals LITERAL
// INTRFC_DECL      -> interface NAME lbrace INTRFC_BODY rbrace semi
// INTRFC_BODY      -> METHOD_DECL | ENUM_DECL | CONSTANT_DECL
// METHOD_DECL      -> NAME [ORDINAL] lparen [PARAM_LIST] rparen semi
// PARAM_LIST       -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL       -> TYPE NAME [ORDINAL]
// TYPE             ->

// Error codes
type errorCode int

const (
	// No error
	E_OK errorCode = iota

	// Unexpected end-of-file
	E_EOF

	E_UNEXPECTED_TOKEN

	// After what appears to be a complete mojom file there were extra tokens.
	E_EXTRANEOUS_TOKEN

	// A simple name was expected but an identifier contained a dot.
	E_EXPECTED_SIMPLE_NAME

	// An attributes section appeared in a location it is not allowed.
	E_BAD_ATTRIBUTE_LOCATION

	E_MISSING_SEMI_COLON
)

type parserError struct {
	code    errorCode
	message string
}

// Make parserError implement errors.error
func (e parserError) Error() string {
	return e.message
}

// ParseNode type
type ParseNode struct {
	name       string
	firstToken *lexer.Token
	parent     *ParseNode
	children   []*ParseNode
}

func (node *ParseNode) String() string {
	return toString(node, 0)
}

// Recursively generates a string representing a tree of nodes
// where indentLevel indicates the level in the tree
func toString(node *ParseNode, indentLevel int) string {
	prefix := "\n" + strings.Repeat(".", indentLevel) + "^"
	firstToken := ""
	if node.firstToken != nil {
		firstToken = fmt.Sprintf("(%s)", node.firstToken.String())
	}
	s := prefix + node.name + firstToken
	if node.children != nil {
		for _, child := range node.children {
			s += toString(child, indentLevel+3)
		}
	}
	return s
}

func newParseNode(name string) *ParseNode {
	node := new(ParseNode)
	node.name = name
	return node
}

func (node *ParseNode) appendChild(name string) *ParseNode {
	child := newParseNode(name)
	child.parent = node
	node.children = append(node.children, child)
	return child
}

// Type Parser
type Parser struct {
	// The stream of input tokens
	inputStream lexer.TokenStream

	// The current error state
	err parserError

	// The root of the parse tree being constructed. This may be nil
	// because the parse tree is only explicitly constructed for
	// debugging purposes.
	rootNode *ParseNode

	// The current node of the parse tree in our recursive descent. This
	// may be nil because the parse tree is only explicitly constructed for
	// debugging purposes.
	currentNode *ParseNode

	// The Parser does not construct the MojomDescriptor. It is populated
	// during the parsing. In this way the same MojomDescriptor may be
	// passed to multiple calls to Parse()
	mojomDescriptor *mojom.MojomDescriptor

	// The Parser creates a new instance of MojomFile on each call to Parse()
	mojomFile *mojom.MojomFile

	debugMode bool
}

func NewParser(fileName string, inputStream lexer.TokenStream,
	descriptorToPopulate *mojom.MojomDescriptor) Parser {
	if descriptorToPopulate == nil {
		panic("descriptorToPopulate must not be nil")
	}
	parser := Parser{inputStream: inputStream, mojomDescriptor: descriptorToPopulate}
	parser.mojomDescriptor = descriptorToPopulate
	parser.mojomFile = parser.mojomDescriptor.AddMojomFile(fileName)
	return parser
}

func ParserForDebugging(inputStream lexer.TokenStream) Parser {
	parser := NewParser("fakeModule", inputStream, mojom.NewMojomDescriptor())
	parser.debugMode = true
	return parser
}

func (p *Parser) GetMojomFile() *mojom.MojomFile {
	return p.mojomFile
}

func (p *Parser) GetParseTree() *ParseNode {
	return p.rootNode
}

// Returns whether or not the current error state os OK
func (p *Parser) OK() bool {
	return p.err.code == E_OK
}

// Returns whether or not there is currently a non-OK error state.
func (p *Parser) Error() bool {
	return p.err.code != E_OK
}

func (p *Parser) ErrorMessage() string {
	return p.err.message
}

// Returns whether or not the most recent error was "EOF"
func (p *Parser) Eof() bool {
	return p.err.code == E_EOF
}

func (p *Parser) pushRootNode(name string) {
	if !p.debugMode {
		return
	}
	p.rootNode = newParseNode(name)
	p.currentNode = p.rootNode
}

func (p *Parser) pushChildNode(name string) {
	if p.currentNode != nil {
		childNode := p.currentNode.appendChild(name)
		p.currentNode = childNode
	}
}

func (p *Parser) popNode() {
	if p.currentNode != nil {
		p.currentNode = p.currentNode.parent
	}
}

// Returns the next available token in the stream without advancing the
// stream cursor. In case the stream cursor is already past the end
// the returned Token will be the EOF token. In this case the global
// error state will be set to E_EOF error code with the message
// "Unexpected end-of-file " concatenated with |eofMessage|. In case of
// any other type of error the returned token is unspecified and the
// global error state will be set with more details.
func (p *Parser) peekNextToken(eofMessage string) (nextToken lexer.Token) {
	nextToken = p.inputStream.PeekNext()
	if nextToken.EOF() {
		errorMessage := "Unexpected end-of-file. " + eofMessage
		p.err = parserError{E_EOF, errorMessage}
	}
	return
}

// This method is similar to peekNextToken except that in the case of EOF
// it does not set the global error state but rather returns |eof| = |true|.
// This method is useful when EOF is an allowed state and you want
// to know what the extraneous token is in case it is not EOF.
func (p *Parser) checkEOF() (eof bool, nextToken lexer.Token) {
	nextToken = p.inputStream.PeekNext()
	eof = nextToken.EOF()
	return
}

// Advances the cursor in the stream and returns true, or else returns
// false if the cursor is already past the end of the stream.
func (p *Parser) consumeNextToken() bool {
	return p.inputStream.ConsumeNext()
}

func (p *Parser) matchToken(expectedKind lexer.TokenKind) (success bool,
	nextToken lexer.Token) {
	if p.Error() {
		return
	}
	message := fmt.Sprintf("I was expecting to find %s next.", expectedKind)
	nextToken = p.peekNextToken(message)
	success = p.OK()
	if success && nextToken.Kind != expectedKind {
		success = false
		message = fmt.Sprintf("Unexpected token at %s: %s. Expecting %s.",
			nextToken.LocationString(), nextToken, expectedKind)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
	}
	if success {
		p.consumeNextToken()
	}
	return
}

func (p *Parser) match(expectedKind lexer.TokenKind) (success bool) {
	success, _ = p.matchToken(expectedKind)
	return
}

func (p *Parser) matchSemicolon(previousToken lexer.Token) bool {
	if p.match(lexer.SEMI) {
		return true
	}
	message := fmt.Sprintf("Missing semicolon after %s at %s.",
		previousToken, previousToken.LocationString())
	p.err = parserError{E_UNEXPECTED_TOKEN, message}
	return false
}

func (p *Parser) readTextToken(kind lexer.TokenKind) (text string, nextToken lexer.Token) {
	if p.Error() {
		return
	}

	success, nextToken := p.matchToken(kind)
	if !success {
		return
	}
	text = nextToken.Text
	return
}

func (p *Parser) readText(kind lexer.TokenKind) (text string) {
	text, _ = p.readTextToken(kind)
	return
}

func (p *Parser) readStringLiteral() string {
	return p.readText(lexer.STRING_LITERAL)
}

func (p *Parser) readStringLiteralToken() (text string, nextToken lexer.Token) {
	return p.readTextToken(lexer.STRING_LITERAL)
}

func (p *Parser) readIdentifier() string {
	return p.readText(lexer.IDENTIFIER)
}

func (p *Parser) readIdentifierToken() (text string, nextToken lexer.Token) {
	return p.readTextToken(lexer.IDENTIFIER)
}

func (p *Parser) readName() string {
	name := p.readIdentifier()
	if strings.Contains(name, ".") {
		// TODO(rudominer) Improve message
		p.err = parserError{E_EXPECTED_SIMPLE_NAME, "Expecting a simple name"}
	}
	return name
}

func (p *Parser) Parse() {
	// Our start symbol is ATTR_MOJOM_FILE
	p.parseAttrMojomFile()

	// Check if there are any extraneous tokens left in the stream.
	if p.OK() {
		eof, nextToken := p.checkEOF()
		if !eof {
			message := fmt.Sprintf("Extraneous token at %s: %v.",
				nextToken.LocationString(), nextToken)
			p.err = parserError{E_EXTRANEOUS_TOKEN, message}
		}
	}
}

// ATTR_MOJOM_FILE  -> ATTRIBUTES MOJOM_FILE | MOJOM_FILE
func (p *Parser) parseAttrMojomFile() {
	if p.Error() {
		return
	}
	p.pushRootNode("AttrMojomFile")
	defer p.popNode()

	initialAttributes := p.parseAttributes()
	p.parseMojomFile(initialAttributes)
}

// MOJOM_FILE   -> MODULE_DECL {IMPORT_STMNT} {MOJOM_DECL}
// MOJOM_FILE   -> IMPORT_STMNT {IMPORT_STMNT} {MOJOM_DECL}
// MOJOM_FILE   -> MOJOM_DECL {ATTR_MOJOM_DECL}
func (p *Parser) parseMojomFile(initialAttributes *mojom.Attributes) {
	if p.Error() {
		return
	}
	p.pushRootNode("MojomFile")
	defer p.popNode()

	moduleDeclExists := p.parseModuleDecl(initialAttributes)
	if p.Error() {
		return
	}

	if moduleDeclExists {
		// If there were initial attributes and a module declaration then the
		// attributes belong to the module declaration and have already
		// been consumed, so we don't need to hold on to them.
		initialAttributes = nil
	}

	importStatementsExist := p.parseImportStatements()
	if p.Error() {
		return
	}

	if p.OK() && !moduleDeclExists && importStatementsExist && initialAttributes != nil {
		message := "Attributes are not allowed before an import statement."
		p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
		return
	}

	attributes := p.parseAttributes()
	if initialAttributes != nil {
		if attributes != nil {
			message := "File starts with two sets of attributes."
			p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			return
		} else {
			attributes = initialAttributes
		}
	}

	// ATTR_MOJOM_DECL  -> [ATTRIBUTES] MOJOM_DECL
	// MOJOM_DECL       -> INTRFC_DECL | STRUCT_DECL | UNION_DECL | ENUM_DECL | CONSTANT_DECL
	for ; ; attributes = p.parseAttributes() {
		if p.Error() {
			return
		}
		eof, nextToken := p.checkEOF()
		if eof {
			if attributes != nil {
				message := "File ends with extranesous attributes."
				p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			return
		}
		switch nextToken.Kind {
		case lexer.INTERFACE:
			p.parseInterfaceDecl(attributes)
		case lexer.STRUCT:
			p.parseStructDecl(attributes)
		case lexer.UNION:
			p.parseUnionDecl(attributes)
		case lexer.ENUM:
			p.parseEnumDecl(attributes)
		case lexer.CONST:
			p.parseConstDecl(attributes)
		default:
			message := fmt.Sprintf("Unexpected token at %s: %s. "+
				"Expecting interface, struct, union, enum or constant.",
				nextToken.LocationString(), nextToken)
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return
		}
	}
}

// ATTRIBUTES       -> lbracket ATTR_ASSIGNMENT { comma, ATTR_ASSIGNMENT}
// ATTR_ASSIGNMENT  -> NAME equals NAME | NAME equals LITERAL
func (p *Parser) parseAttributes() (attributes *mojom.Attributes) {
	if p.Error() {
		return
	}

	eof, nextToken := p.checkEOF()
	if eof || nextToken.Kind != lexer.LBRACKET {
		// There is no attributes section here
		return
	}
	p.consumeNextToken() // consume the LBRACKET

	p.pushChildNode("attributes")
	defer p.popNode()

	attributes = mojom.NewAttributes()

	for nextToken.Kind != lexer.RBRACKET {
		key := p.readName()
		if p.Error() {
			return
		}
		p.match(lexer.EQUALS)
		if p.Error() {
			return
		}
		value := p.readName()
		if p.Error() {
			return
		}
		attributes.List = append(attributes.List, mojom.MojomAttribute{key, value})

		nextToken = p.peekNextToken("I was reading an attributes section.")
		if p.Error() {
			return
		}
		p.consumeNextToken()
		if nextToken.Kind != lexer.RBRACKET && nextToken.Kind != lexer.COMMA {
			var message string
			switch nextToken.Kind {
			case lexer.MODULE, lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM:
				message = fmt.Sprintf("The attribute section is missing a closing ] before %v at %s.",
					nextToken, nextToken.LocationString())
			default:
				message = fmt.Sprintf("Unexpected token in attributes section at %s: %v. Expecting comma or ].",
					nextToken.LocationString(), nextToken)

			}
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return
		}
	}

	return
}

func (p *Parser) parseModuleDecl(attributes *mojom.Attributes) bool {
	if p.Error() {
		return false
	}
	nextToken := p.peekNextToken("No Mojom declarations found.")
	switch nextToken.Kind {
	case lexer.MODULE:
		p.consumeNextToken() // consume the MODULE token.
		break
	case lexer.IMPORT, lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return false
	default:
		message := fmt.Sprintf("Unexpected token at %s: %s. "+
			"Expecting module, import, interface, struct, union, enum or constant.",
			nextToken.LocationString(), nextToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return false
	}

	p.pushChildNode("moduleDecl")
	defer p.popNode()

	identifier, identifierToken := p.readIdentifierToken()
	if p.Error() {
		return true
	}
	p.matchSemicolon(identifierToken)
	if p.Error() {
		return true
	}
	p.mojomFile.ModuleNamespace = identifier
	p.mojomFile.Attributes = attributes
	return true
}

func (p *Parser) parseImportStatements() (atLeastOneImport bool) {
	if p.Error() {
		return false
	}

	nextToken := p.peekNextToken("No Mojom declarations found.")
	for nextToken.Kind == lexer.IMPORT {
		atLeastOneImport = true
		p.pushChildNode("importStmnt")
		p.consumeNextToken() // consume the IMPORT token.

		fileName, fileNameToken := p.readStringLiteralToken()
		if p.Error() {
			return
		}

		p.matchSemicolon(fileNameToken)
		if p.Error() {
			return
		}
		p.mojomFile.AddImport(fileName)

		nextToken = p.peekNextToken("No Mojom declarations found.")
		p.popNode()
		if p.Error() {
			return true
		}
	}

	switch nextToken.Kind {
	case lexer.MODULE:
		message := "The module declaration must come before the import statements."
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return true
	case lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return true
	default:
		message := fmt.Sprintf("Unexpected token at %s: %s. "+
			"Expecting import, interface, struct, union, enum or constant.",
			nextToken.LocationString(), nextToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return false
	}

	return
}

func (p *Parser) parseInterfaceDecl(attributes *mojom.Attributes) {
	if p.Error() {
		return
	}
	p.pushChildNode("interfaceDecl")
	defer p.popNode()

	p.match(lexer.INTERFACE)
	simpleName := p.readName()
	if p.Error() {
		return
	}

	mojomInterface := p.mojomFile.AddInterface(simpleName, attributes)

	p.match(lexer.LBRACE)
	if p.Error() {
		return
	}
	p.parseInterfaceBody(mojomInterface)
	if p.Error() {
		return
	}
	_, braceToken := p.matchToken(lexer.RBRACE)
	if p.Error() {
		return
	}
	p.matchSemicolon(braceToken)
	if p.Error() {
		return
	}
}

func (p *Parser) parseInterfaceBody(mojomInterface *mojom.MojomInterface) {
	if p.Error() {
		return
	}
	p.pushChildNode("interfaceBody")
	defer p.popNode()
}

func (p *Parser) parseStructDecl(attributes *mojom.Attributes) {
	if p.Error() {
		return
	}
}

func (p *Parser) parseUnionDecl(attributes *mojom.Attributes) {
	if p.Error() {
		return
	}
}

func (p *Parser) parseEnumDecl(attributes *mojom.Attributes) {
	if p.Error() {
		return
	}
}

func (p *Parser) parseConstDecl(attributes *mojom.Attributes) {
	if p.Error() {
		return
	}
}
