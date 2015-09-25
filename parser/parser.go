package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"github.com/rudominer/mojom_parser/mojom"
	"strings"
)

///////////////////////////////////////////////////////////////////////
/// Type Parser
/// //////////////////////////////////////////////////////////////////

// Type Parser
type Parser struct {
	// The stream of input tokens
	inputStream lexer.TokenStream

	// The current error state
	err parserError

	lastSeen     lexer.Token
	lastConsumed lexer.Token

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

	// The top of the Scope stack.
	currentScope *mojom.Scope

	debugMode bool
}

func NewParser(fileName, fileContents string,
	descriptorToPopulate *mojom.MojomDescriptor) Parser {
	if descriptorToPopulate == nil {
		panic("descriptorToPopulate must not be nil")
	}
	inputStream := lexer.Tokenize(fileContents)
	parser := Parser{inputStream: inputStream, mojomDescriptor: descriptorToPopulate}
	parser.mojomDescriptor = descriptorToPopulate
	parser.mojomFile = parser.mojomDescriptor.AddMojomFile(fileName)
	return parser
}

func (p *Parser) SetDebugMode(debug bool) {
	p.debugMode = debug
}

func (p *Parser) Parse() {
	// Our start symbol is ATTR_MOJOM_FILE
	p.parseMojomFile()

	// Check if there are any extraneous tokens left in the stream.
	if p.OK() && !p.checkEOF() {
		token := p.peekNextToken("")
		message := fmt.Sprintf("Extraneous token at %s: %v.",
			token.LongLocationString(), token)
		p.err = parserError{E_EXTRANEOUS_TOKEN, message}
	}
}

func ParserForDebugging(fileContents string) Parser {
	parser := NewParser("fakeModule", fileContents, mojom.NewMojomDescriptor())
	parser.debugMode = true
	return parser
}

func (p *Parser) GetMojomFile() *mojom.MojomFile {
	return p.mojomFile
}

func (p *Parser) GetMojomDescriptor() *mojom.MojomDescriptor {
	return p.mojomDescriptor
}

func (p *Parser) GetParseTree() *ParseNode {
	return p.rootNode
}

// Returns whether or not the current error state os OK
func (p *Parser) OK() bool {
	return p.err.code == E_OK
}

func (p *Parser) Error() error {
	return p.err
}

func (p *Parser) ErrorMessage() string {
	return p.err.message
}

// Returns whether or not the most recent error was "EOF"
func (p *Parser) Eof() bool {
	return p.err.code == E_EOF
}

//////////// Error codes //////////
type errorCode int

const (
	// No error
	E_OK errorCode = iota

	// Unexpected end-of-file
	E_EOF

	E_DUPLICATE_DECLARATION

	E_UNEXPECTED_TOKEN

	// After what appears to be a complete mojom file there were extra tokens.
	E_EXTRANEOUS_TOKEN

	// A simple name was expected but an identifier contained a dot.
	E_EXPECTED_SIMPLE_NAME

	// An attributes section appeared in a location it is not allowed.
	E_BAD_ATTRIBUTE_LOCATION

	E_MISSING_SEMI_COLON

	E_INTEGER_OUT_OF_RANGE

	E_INTEGER_PARSE_ERROR

	E_TYPE_NOT_ASSIGNMENT_COMPATIBLE
)

type parserError struct {
	code    errorCode
	message string
}

// Make parserError implement the error interface.
func (e parserError) Error() string {
	return e.message
}

////////////////// Parse Tree /////////////////////

///// ParseNode type /////
type ParseNode struct {
	name     string
	tokens   []*lexer.Token
	parent   *ParseNode
	children []*ParseNode
}

func (node *ParseNode) String() string {
	return toString(node, 0)
}

// Recursively generates a string representing a tree of nodes
// where indentLevel indicates the level in the tree
func toString(node *ParseNode, indentLevel int) string {
	prefix := "\n" + strings.Repeat(".", indentLevel) + "^"
	firstTokens := ""
	if node.tokens != nil {
		firstTokens = fmt.Sprintf("%s", node.tokens)
	}
	s := fmt.Sprintf("%s%s%s", prefix, node.name, firstTokens)
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

func (node *ParseNode) appendChild(name string, firstToken *lexer.Token) *ParseNode {
	child := newParseNode(name)
	child.tokens = append(child.tokens, firstToken)
	child.parent = node
	node.children = append(node.children, child)
	return child
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
		tokenCopy := p.lastSeen
		childNode := p.currentNode.appendChild(name, &(tokenCopy))
		p.currentNode = childNode
	}
}

func (p *Parser) attachToken() {
	if p.currentNode != nil {
		tokenCopy := p.lastSeen
		p.currentNode.tokens = append(p.currentNode.tokens, &tokenCopy)
	}
}

func (p *Parser) popNode() {
	if p.currentNode != nil {
		p.currentNode = p.currentNode.parent
	}
}
