package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"github.com/rudominer/mojom_parser/mojom"
	"strings"
)

// NOTE(rudominer) Work in progress...

// Upper case means non-terminals.
// Lower case means terminals and refers to the TokenKind enum in lexer.go
//
// MOJOM_FILE       -> [MODULE_DECL] {IMPORT_STMNT} {MOJOM_DECL}
// MODULE_DECL      -> [ATTRIBUTES] module identifier semi
// IMPORT_STMNT     -> import string_literal semi
// MOJOM_DECL       -> INTRFC_DECL | STRUCT_DECL | UNION_DECL | ENUM_DECL | CONSTANT_DECL
// ATTRIBUTES       -> lbracket ATTR_ASSIGNMENT { comma, ATTR_ASSIGNMENT}
// ATTR_ASSIGNMENT  -> NAME equals NAME | NAME equals LITERAL
// INTRFC_DECL      -> [ATTRIBUTES] interface NAME lbrace INTRFC_BODY rbrace semi
// INTRFC_BODY      -> METHOD_DECL | ENUM_DECL | CONSTANT_DECL
// METHOD_DECL      -> [ATTRIBUTES] NAME [ORDINAL] lparen [PARAM_LIST] rparen semi
// PARAM_LIST       -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL       -> [ATTRIBUTES] TYPE NAME [ORDINAL]
// TYPE             ->

// Error codes
type errorCode int

const (
	// No error
	E_OK errorCode = iota

	// Unexpected end-of-file
	E_EOF

	// Expected token was not matched
	E_FAILED_TOKEN_MATCH

	// After what appears to be a complete mojom file there were extra tokens.
	E_EXTRANEOUS_TOKEN

	// A simple name was expected but an identifier contained a dot.
	E_EXPECTED_SIMPLE_NAME
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

	mojomDescriptor *mojom.MojomDescriptor

	mojomFile *mojom.MojomFile
}

func NewParser(moduleName string, inputStream lexer.TokenStream,
	descriptorToPopulate *mojom.MojomDescriptor) {
	parser := Parser{inputStream: inputStream, mojomDescriptor: descriptorToPopulate}
	parser.mojomFile = new(mojom.MojomFile)
	parser.mojomFile.Descriptor = parser.mojomDescriptor
	parser.mojomFile.ModuleName = moduleName
}

func ParserForDebugging(inputStream lexer.TokenStream) Parser {
	parser := Parser{inputStream: inputStream}
	parser.rootNode = newParseNode("MojomFile")
	parser.currentNode = parser.rootNode
	return parser
}

func (p *Parser) Parse() {
	p.parseInterfaceDecl(mojom.MojomAttributes{})

	// Check if there are any extraneous tokens left in the stream.
	if p.OK() {
		nextToken, eof := p.expectEOF()
		if !eof {
			message := fmt.Sprintf("Extraneous token at %s: %v.",
				nextToken.LocationString(), nextToken)
			p.err = parserError{E_EXTRANEOUS_TOKEN, message}
		}
	}
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
// This method is useful when EOF is the expected state and you want
// to know what the extraneous token is in case it is not EOF.
func (p *Parser) expectEOF() (nextToken lexer.Token, eof bool) {
	nextToken = p.inputStream.PeekNext()
	eof = nextToken.EOF()
	return
}

// Advances the cursor in the stream and returns true, or else returns
// false if the cursor is already past the end of the stream.
func (p *Parser) consumeNextToken() bool {
	return p.inputStream.ConsumeNext()
}

func (p *Parser) match(expectedKind lexer.TokenKind) (success bool,
	nextToken lexer.Token) {
	if p.Error() {
		return
	}
	message := fmt.Sprintf("I was expecting to find %s next.", expectedKind)
	nextToken = p.peekNextToken(message)
	success = p.OK()
	if success && nextToken.Kind != expectedKind {
		success = false
		// TODO add location info to error message.
		message = fmt.Sprintf("Unexpected token at %s: %s. Expecting %s.",
			nextToken.LocationString(), nextToken, expectedKind)
		p.err = parserError{E_FAILED_TOKEN_MATCH, message}
	}
	if success {
		p.consumeNextToken()
	}
	return
}

func (p *Parser) readName() (name string) {
	if p.Error() {
		return
	}

	_, nextToken := p.match(lexer.IDENTIFIER)
	if p.Error() {
		return
	}
	name = nextToken.Text
	if strings.Contains(name, ".") {
		// TODO(rudominer) Improve message
		p.err = parserError{E_EXPECTED_SIMPLE_NAME, "Expecting a simple name"}
	}
	return
}

func (p *Parser) recordInterfaceName(name string,
	attributes mojom.MojomAttributes) (mojomInterface *mojom.MojomInterface) {
	if p.Error() {
		return
	}
	declarationData := new(mojom.DeclarationData)
	declarationData.Name = name
	declarationData.Attributes = attributes
	mojomInterface = new(mojom.MojomInterface)
	mojomInterface.SetDeclarationData(declarationData)
	// TODO(record this in the symbol table)
	return
}

func (p *Parser) parseInterfaceBody(mojomInterface *mojom.MojomInterface) {
	if p.Error() {
		return
	}
	p.pushChildNode("interfaceBody")
	defer p.popNode()
}

func (p *Parser) parseInterfaceDecl(interfaceAttributes mojom.MojomAttributes) {
	if p.Error() {
		return
	}
	p.pushChildNode("interfaceDecl")
	defer p.popNode()

	p.match(lexer.INTERFACE)
	name := p.readName()
	mojomInterface := p.recordInterfaceName(name, interfaceAttributes)
	p.match(lexer.LBRACE)
	p.parseInterfaceBody(mojomInterface)
	p.match(lexer.RBRACE)
	p.match(lexer.SEMI)
}
