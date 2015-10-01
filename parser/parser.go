package parser

import (
	"fmt"
	"mojo/public/tools/bindings/mojom_parser/lexer"
	"mojo/public/tools/bindings/mojom_parser/mojom"
	"strings"
)

///////////////////////////////////////////////////////////////////////
/// Type Parser
/// //////////////////////////////////////////////////////////////////

// This file contains the definition of the Parser type but it does not contain
// the actual parsing logic. That may be found in parsing.go

// A Parser is constructed and used to parse a single mojom file. The
// Parser is given a pointer to a MojomDescriptor that it will populate.
// The same MojomDescriptor may be given to successive runs of the Parser
// so that an entire graph of .mojom files may be parsed.
type Parser struct {
	// The stream of input tokens
	inputStream lexer.TokenStream

	// The current error state. In the current generation of the Parser we
	// only handle a single parse error before giving up. If an error
	// has been encountered then |err| is not nil.
	// TODO(rudominer) Enhancement: Parser should be able to keep going
	// after some errors. Change this field to be a list of errors instead of
	// a single error.
	err *ParseError

	// Last token seen, whether or not it was consumed.
	lastSeen lexer.Token

	// Last token consumed. May or may not be equal to lastSeen.
	lastConsumed lexer.Token

	// The root of the parse tree being constructed. This may be nil
	// because the parse tree is only explicitly constructed in
	// debug mode.
	rootNode *ParseNode

	// The current node of the parse tree in our recursive descent. This
	// may be nil because the parse tree is only explicitly constructed fo
	// in debug mode.
	currentNode *ParseNode

	// The MojomDescriptor being filled in by the Parser. This is passed
	// in in the constructor.
	mojomDescriptor *mojom.MojomDescriptor

	// The Parser creates a new instance of MojomFile on each call to Parse(),
	// which may only be called once per instance.
	mojomFile *mojom.MojomFile

	// The top of the Scope stack.
	currentScope *mojom.Scope

	debugMode bool
	used      bool
}

// Make a new Parser in preparation for calling Parse().
func MakeParser(fileName, fileContents string,
	descriptorToPopulate *mojom.MojomDescriptor) Parser {
	if descriptorToPopulate == nil {
		panic("descriptorToPopulate must not be nil")
	}
	inputStream := lexer.Tokenize(fileContents)
	parser := Parser{inputStream: inputStream,
		mojomDescriptor: descriptorToPopulate}
	parser.mojomDescriptor = descriptorToPopulate
	parser.mojomFile = parser.mojomDescriptor.AddMojomFile(fileName)
	return parser
}

func (p *Parser) SetDebugMode(debug bool) {
	p.debugMode = debug
}

// Perform the parsing on the |fileContents| passed to MakeParser().
// The |descriptorToPopulate| passed to MakeParser() will be populated.
// After Parse() is done call GetMojomFile() to get the resulting
// |MojomFile|. The |Imports| field of that |MojomFile| gives the
// files imported by the file that was just parsed. For each file |f|
// in |Imports|, call MojomDescriptor.ContainsFile(f) on
// |descriptorToPopulate| to determine whether or not |f| has already
// been parsed. If not then construct another Parser for |f| and its
// contents and call Parse() again.
func (p *Parser) Parse() {
	if p.used {
		panic("An instance of Parser may only be used once.")
	}
	p.used = true

	// Perform the recursive descent.
	p.parseMojomFile()

	// Check if there are any extraneous tokens left in the stream.
	if p.OK() && !p.checkEOF() {
		token := p.peekNextToken("")
		message := fmt.Sprintf("Extraneous token at %s: %v.",
			token.LongLocationString(), token)
		p.err = &ParseError{E_EXTRANEOUS_TOKEN, message}
	}
}

// After Parse() is done call this method to obtain the resulting
// MojomFile.
func (p *Parser) GetMojomFile() *mojom.MojomFile {
	return p.mojomFile
}

// Returns the root of the parse tree if this Parser is in debug mode.
// Otherwise returns nil.
func (p *Parser) GetParseTree() *ParseNode {
	return p.rootNode
}

////////////////////////////////////////////////////////////////////////////
// Parse Error Handling
////////////////////////////////////////////////////////////////////////////

type ParseError struct {
	code    ParseErrorCode
	message string
}

// Make ParseError implement the error interface.
func (e ParseError) Error() string {
	return e.message
}

// Returns whether or not the Parser is in a non-error state.
func (p *Parser) OK() bool {
	return p.err == nil
}

// Returns the current ParseError or nil if OK() is true.
func (p *Parser) GetError() *ParseError {
	return p.err
}

//////////// Error codes //////////
type ParseErrorCode int

const (
	// An attributes section appeared in a location it is not allowed.
	E_BAD_ATTRIBUTE_LOCATION = iota

	// Two types or two values with the same fully qualified name were declared.
	E_DUPLICATE_DECLARATION

	// Unexpected end-of-file
	E_EOF

	// A simple name was expected but an identifier contained a dot.
	E_EXPECTED_SIMPLE_NAME

	// After what appears to be a complete mojom file there were extra tokens.
	E_EXTRANEOUS_TOKEN

	// An integer literal value was too large
	E_INTEGER_OUT_OF_RANGE

	// An integer literal value was ill-formed.
	// TODO(azani) This is only necessary because the lexer allows some
	// illegal tokens such as "0x"
	E_INTEGER_PARSE_ERROR

	// A semicolon was missing.
	// TODO(rudominer) Consider elimintating most semicolons from the language.
	E_MISSING_SEMI_COLON

	// The type of a value is not compatible with the type of the variable
	// to which it is being assigned.
	E_TYPE_NOT_ASSIGNMENT_COMPATIBLE

	// An unexpected token was encountered. This is the most common error.
	E_UNEXPECTED_TOKEN
)

////////////////////////////////////////////////////////////////////////////
// Parse Tree Support
////////////////////////////////////////////////////////////////////////////

// In normal operation we do not explicit construct a parse tree. This is
// only used in debug mode.

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
