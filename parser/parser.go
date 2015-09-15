package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"github.com/rudominer/mojom_parser/mojom"
)

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

// Type Parser
type Parser struct {
	// The stream of input tokens
	inputStream lexer.TokenStream

	// The current error state
	err parserError

	lastPeek lexer.Token

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

func (p *Parser) Parse() {
	// Our start symbol is ATTR_MOJOM_FILE
	p.parseMojomFile()

	// Check if there are any extraneous tokens left in the stream.
	if p.OK() && !p.checkEOF() {
		token := p.lastPeek
		message := fmt.Sprintf("Extraneous token at %s: %v.",
			token.LocationString(), token)
		p.err = parserError{E_EXTRANEOUS_TOKEN, message}
	}
}
