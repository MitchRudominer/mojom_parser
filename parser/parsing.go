package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"github.com/rudominer/mojom_parser/mojom"
	"strconv"
	"strings"
)

// The code in this file implements a recursive-descent, predictive parser
// for the context-free grammar listed below.
//
// The grammar is similar to the grammar in the document "Mojom Language
// Specification", but it has been modified in order to make it LL(1). This
// is necessary in order to be able to use it to do predictive top-down
// parsing. (See Section 4.4.3 of "Compilers: Principles, Techniques and Tools"
// 2nd Edition, by Aho et al."). This requirement explains the slight awkwardness
// seen below regarding the handling of Mojom attributes.
//
// Our recursive descent logic is implemented in the methods below with names of
// the form "parseX()" where "X" is (an altered spelling of) one of our
// non-terminal symbols. Each of the productions below has been copied to the
// piece of code responsible for implementing the logic associated with the
//production.
//
// Key:
// Upper case means non-terminals.
// Lower case means terminals and refers to the TokenKind enum in lexer.go
// Vertical bar | means alternatives.
// Braces {} means zero or more.
// Brackets [] means zero or one.
//
// ATTR_MOJOM_FILE      -> [ATTRIBUTES] MOJOM_FILE
// MOJOM_FILE           -> MODULE_DECL {IMPORT_STMNT} {ATTR_MOJOM_DECL}
// MOJOM_FILE           -> IMPORT_STMNT {IMPORT_STMNT} {ATTR_MOJOM_DECL}
// MOJOM_FILE           -> MOJOM_DECL {ATTR_MOJOM_DECL}

// MODULE_DECL          -> module IDENTIFIER semi
// IMPORT_STMNT         -> import string_literal

// ATTR_MOJOM_DECL      -> [ATTRIBUTES] MOJOM_DECL
// MOJOM_DECL           -> INTRFC_DECL | STRUCT_DECL | UNION_DECL | ENUM_DECL | CONSTANT_DECL

// ATTRIBUTES           -> lbracket ATTR_ASSIGNMENT { comma, ATTR_ASSIGNMENT}
// ATTR_ASSIGNMENT      -> name equals name | name equals string_literal

// INTRFC_DECL          -> interface name lbrace INTRFC_BODY rbrace semi
// INTRFC_BODY          -> {ATTR_INTRFC_ELEMENT}
// ATTR_INTRFC_ELEMENT  -> [ATTRIBUTES] INTRFC_ELEMENT
// INTRFC_ELEMENT       -> METHOD_DECL | ENUM_DECL | CONSTANT_DECL

// METHOD_DECL          -> name [ORDINAL] lparen [PARAM_LIST] rparen [response lparen [PARAM_LIST] rparen]semi
// PARAM_LIST           -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL           -> [ATTRIBUTES] TYPE NAME [ORDINAL]

// STRUCT_DECL          -> struct name lbrace STRUCT_BODY rbrace semi
// STRUCT_BODY          -> {ATTR_STRUCT_ELEMENT}
// ATTR_STRUCT_ELEMENT  -> [ATTRIBUTES] STRUCT_ELEMENT
// STRUCT_ELEMENT       -> STRUCT_FIELD | ENUM_DECL | CONSTANT_DECL

////////////////////////////////////////////////////////////////////////////
// ParseX() methods follow.
////////////////////////////////////////////////////////////////////////////

// ATTR_MOJOM_FILE  -> [ATTRIBUTES] MOJOM_FILE
// MOJOM_FILE       -> MODULE_DECL {IMPORT_STMNT} {ATTR_MOJOM_DECL}
// MOJOM_FILE       -> IMPORT_STMNT {IMPORT_STMNT} {ATTR_MOJOM_DECL}
// MOJOM_FILE       -> MOJOM_DECL {ATTR_MOJOM_DECL}
func (p *Parser) parseMojomFile() bool {
	if p.Error() {
		return false
	}
	p.pushRootNode("MojomFile")
	defer p.popNode()

	initialAttributes := p.parseAttributes()
	if p.Error() {
		return false
	}

	moduleDeclExists := p.parseModuleDecl(initialAttributes)
	if p.Error() {
		return false
	}

	if moduleDeclExists {
		// If there were initial attributes and a module declaration then the
		// attributes belong to the module declaration and have already
		// been consumed, so we don't need to hold on to them.
		initialAttributes = nil
	}

	importStatementsExist := p.parseImportStatements()
	if p.Error() {
		return false
	}

	if !moduleDeclExists && importStatementsExist && initialAttributes != nil {
		message := "Attributes are not allowed before an import statement."
		p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
		return false
	}

	attributes := p.parseAttributes()
	if p.Error() {
		return false
	}
	if initialAttributes != nil {
		if attributes != nil {
			message := "File starts with two sets of attributes."
			p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			return false
		} else {
			attributes = initialAttributes
		}
	}

	// ATTR_MOJOM_DECL  -> [ATTRIBUTES] MOJOM_DECL
	// MOJOM_DECL       -> INTRFC_DECL | STRUCT_DECL | UNION_DECL | ENUM_DECL | CONSTANT_DECL
	for ; ; attributes = p.parseAttributes() {
		if p.Error() {
			return false
		}
		if p.checkEOF() {
			if attributes != nil {
				message := "File ends with extranesous attributes."
				p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			return false
		}
		nextToken := p.lastPeek
		switch nextToken.Kind {
		case lexer.INTERFACE:
			mojomInterface := p.parseInterfaceDecl(attributes)
			if p.OK() {
				p.mojomFile.AddInterface(mojomInterface)
			}
		case lexer.STRUCT:
			mojomStruct := p.parseStructDecl(attributes)
			if p.OK() {
				p.mojomFile.AddStruct(mojomStruct)
			}
		case lexer.UNION:
			p.parseUnionDecl(attributes)
		case lexer.ENUM:
			p.parseEnumDecl(attributes)
		case lexer.CONST:
			p.parseConstDecl(attributes)
		default:
			message := fmt.Sprintf("Unexpected token at %s: %s. "+
				"Expecting interface, struct, union, enum or const.",
				nextToken.LocationString(), nextToken)
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return false
		}
	}
	return p.OK()
}

// ATTRIBUTES      -> lbracket ATTR_ASSIGNMENT { comma, ATTR_ASSIGNMENT}
// ATTR_ASSIGNMENT -> name equals name | name equals string_literal
func (p *Parser) parseAttributes() (attributes *mojom.Attributes) {
	if p.Error() {
		return
	}

	if !p.tryMatch(lexer.LBRACKET) {
		// There is no attributes section here
		return
	}

	p.pushChildNode("attributes")
	defer p.popNode()

	attributes = mojom.NewAttributes()

	nextToken := p.lastPeek
	for nextToken.Kind != lexer.RBRACKET {
		key := p.readName()
		if p.Error() {
			return
		}
		if !p.match(lexer.EQUALS) {
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

//MODULE_DECL -> module identifier semi
func (p *Parser) parseModuleDecl(attributes *mojom.Attributes) (moduleDeclExists bool) {
	moduleDeclExists = false
	if p.Error() {
		return
	}
	nextToken := p.peekNextToken("No Mojom declarations found.")
	switch nextToken.Kind {
	case lexer.MODULE:
		moduleDeclExists = true
		p.consumeNextToken() // consume the MODULE token.
		break
	case lexer.IMPORT, lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return
	default:
		message := fmt.Sprintf("Unexpected token at %s: %s. "+
			"Expecting module, import, interface, struct, union, enum or constant.",
			nextToken.LocationString(), nextToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return
	}

	p.pushChildNode("moduleDecl")
	defer p.popNode()

	identifier := p.readIdentifier()
	if p.Error() {
		return
	}
	if !p.matchSemicolon() {
		return
	}

	p.mojomFile.ModuleNamespace = identifier
	p.mojomFile.Attributes = attributes
	return
}

// IMPORT_STMNT  -> import string_literal
func (p *Parser) parseImportStatements() (atLeastOneImport bool) {
	atLeastOneImport = false
	if p.Error() {
		return
	}

	nextToken := p.peekNextToken("No Mojom declarations found.")
	for nextToken.Kind == lexer.IMPORT {
		atLeastOneImport = true
		p.pushChildNode("importStmnt")
		p.consumeNextToken() // consume the IMPORT token.

		fileName := p.readStringLiteral()
		if p.Error() {
			return
		}

		if !p.matchSemicolon() {
			return
		}
		p.mojomFile.AddImport(fileName)

		nextToken = p.peekNextToken("No Mojom declarations found.")
		p.popNode()
		if p.Error() {
			return
		}
	}

	switch nextToken.Kind {
	case lexer.MODULE:
		message := "The module declaration must come before the import statements."
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return
	case lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return
	default:
		message := fmt.Sprintf("Unexpected token at %s: %s. "+
			"Expecting import, interface, struct, union, enum or constant.",
			nextToken.LocationString(), nextToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return
	}

}

// INTRFC_DECL  -> interface name lbrace INTRFC_BODY rbrace semi
func (p *Parser) parseInterfaceDecl(attributes *mojom.Attributes) (mojomInterface *mojom.MojomInterface) {
	if p.Error() {
		return
	}
	p.pushChildNode("interfaceDecl")
	defer p.popNode()

	if !p.match(lexer.INTERFACE) {
		return
	}

	simpleName := p.readName()
	if p.Error() {
		return
	}

	mojomInterface = mojom.NewMojomInterface(simpleName, attributes, p.mojomFile, p.mojomDescriptor)

	if !p.match(lexer.LBRACE) {
		return
	}

	if !p.parseInterfaceBody(mojomInterface) {
		return
	}

	if !p.match(lexer.RBRACE) {
		return
	}

	p.matchSemicolon()

	return
}

// INTRFC_BODY          -> {ATTR_INTRFC_ELEMENT}
// ATTR_INTRFC_ELEMENT  -> [ATTRIBUTES] INTRFC_ELEMENT
// INTRFC_ELEMENT       -> METHOD_DECL | ENUM_DECL | CONSTANT_DECL
func (p *Parser) parseInterfaceBody(mojomInterface *mojom.MojomInterface) bool {
	if p.Error() {
		return p.OK()
	}
	p.pushChildNode("interfaceBody")
	defer p.popNode()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if p.Error() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing an interface body.")
		switch nextToken.Kind {
		case lexer.NAME:
			method := p.parseMethodDecl(attributes)
			if p.OK() {
				mojomInterface.AddMethod(method)
			}
		case lexer.ENUM:
			enum := p.parseEnumDecl(attributes)
			if p.OK() {
				mojom.AddEnum(mojomInterface, enum)
			}
		case lexer.CONST:
			declaredConst := p.parseConstDecl(attributes)
			if p.OK() {
				mojom.AddDeclaredConstant(mojomInterface, declaredConst)
			}
		case lexer.RBRACE:
			rbraceFound = true
			if attributes != nil {
				message := "Interface body ends with extranesous attributes."
				p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			break
		default:
			message := fmt.Sprintf("Unexpected token within interface body at %s: %s. "+
				"Expecting union, enum, const or an identifier",
				nextToken.LocationString(), nextToken)
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return false
		}
	}
	if p.OK() {
		mojomInterface.ComputeMethodOrdinals()
	}
	return p.OK()
}

// METHOD_DECL -> name [ORDINAL] lparen [PARAM_LIST] rparen [response lparen [PARAM_LIST] rparen]semi
func (p *Parser) parseMethodDecl(attributes *mojom.Attributes) *mojom.MojomMethod {
	if p.Error() {
		return nil
	}
	p.pushChildNode("methodDecl")
	defer p.popNode()

	methodName := p.readName()
	if p.Error() {
		return nil
	}

	ordinalValue := -p.parserOrdinal()

	if !p.match(lexer.LPAREN) {
		return nil
	}

	params := p.parseParamList()
	if p.Error() {
		return nil
	}

	if !p.match(lexer.RPAREN) {
		return nil
	}
	rParenBeforeSemicolon := p.lastPeek

	// Check for a response message
	var responseParams *mojom.MojomStruct = nil
	if p.tryMatch(lexer.RESPONSE) {
		if !p.match(lexer.LPAREN) {
			return nil
		}

		responseParams = p.parseParamList()
		if p.Error() {
			return nil
		}

		if !p.match(lexer.RPAREN) {
			return nil
		}
		rParenBeforeSemicolon = p.lastPeek
	}

	if !p.matchSemicolonToken(rParenBeforeSemicolon) {
		return nil
	}

	mojomMethod := mojom.NewMojomMethod(methodName, ordinalValue, params, responseParams)
	return mojomMethod
}

// PARAM_LIST -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL -> [ATTRIBUTES] TYPE name [ORDINAL]
func (p *Parser) parseParamList() (paramStruct *mojom.MojomStruct) {
	p.pushChildNode("paramList")
	defer p.popNode()

	paramStruct = mojom.NewMojomStruct("ParamStruct", nil, nil, nil)
	nextToken := p.peekNextToken("I was parsing method parameters.")
	for nextToken.Kind != lexer.RPAREN {
		if p.Error() {
			return
		}
		attributes := p.parseAttributes()
		if p.Error() {
			return
		}
		fieldType := p.parseType()
		if p.Error() {
			return
		}
		name := p.readName()
		if p.Error() {
			return
		}
		ordinalValue := p.parserOrdinal()

		paramStruct.AddField(mojom.BuildStructField(fieldType, name, ordinalValue, attributes, nil))

		nextToken = p.peekNextToken("I was parsing method parameters.")
		switch nextToken.Kind {
		case lexer.COMMA:
			p.consumeNextToken()
			continue
		case lexer.RPAREN:
			continue
		default:
			message := fmt.Sprintf("Unexpected token within method parameters at %s: %s. "+
				"Expecting comma or ).", nextToken.LocationString(), nextToken)
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return nil
		}
	}
	return
}

// STRUCT_DECL   -> struct name lbrace STRUCT_BODY rbrace semi
func (p *Parser) parseStructDecl(attributes *mojom.Attributes) (mojomStruct *mojom.MojomStruct) {
	if p.Error() {
		return
	}
	p.pushChildNode("structDecl")
	defer p.popNode()

	if !p.match(lexer.STRUCT) {
		return
	}

	simpleName := p.readName()
	if p.Error() {
		return
	}

	mojomStruct = mojom.NewMojomStruct(simpleName, attributes,
		p.mojomFile, p.mojomDescriptor)

	if !p.match(lexer.LBRACE) {
		return
	}

	if !p.parseStructBody(mojomStruct) {
		return
	}

	if !p.match(lexer.RBRACE) {
		return
	}

	p.matchSemicolon()

	return
}

// STRUCT_BODY          -> {ATTR_STRUCT_ELEMENT}
// ATTR_STRUCT_ELEMENT  -> [ATTRIBUTES] STRUCT_ELEMENT
// STRUCT_ELEMENT       -> STRUCT_FIELD | ENUM_DECL | CONSTANT_DECL
func (p *Parser) parseStructBody(mojomStruct *mojom.MojomStruct) bool {
	if p.Error() {
		return p.OK()
	}
	p.pushChildNode("structBody")
	defer p.popNode()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if p.Error() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing a struct body.")
		switch nextToken.Kind {
		case lexer.NAME:
			field := p.parseStructField(attributes)
			if p.OK() {
				mojomStruct.AddField(field)
			}
		case lexer.ENUM:
			enum := p.parseEnumDecl(attributes)
			if p.OK() {
				mojom.AddEnum(mojomStruct, enum)
			}
		case lexer.CONST:
			declaredConst := p.parseConstDecl(attributes)
			if p.OK() {
				mojom.AddDeclaredConstant(mojomStruct, declaredConst)
			}
		case lexer.RBRACE:
			rbraceFound = true
			if attributes != nil {
				message := "Struct body ends with extranesous attributes."
				p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			break
		default:
			message := fmt.Sprintf("Unexpected token within struct body at %s: %s. "+
				"Expecting field, enum or constant declaration.",
				nextToken.LocationString(), nextToken)
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return false
		}
	}
	if p.OK() {
		mojomStruct.ComputeFieldOrdinals()
	}
	return p.OK()
}

func (p *Parser) parseStructField(attributes *mojom.Attributes) (structField mojom.StructField) {
	return
	//TODO
}

func (p *Parser) parseUnionDecl(attributes *mojom.Attributes) (union *mojom.MojomUnion) {
	if p.Error() {
		return
	}
	// TODO
	return
}

func (p *Parser) parseEnumDecl(attributes *mojom.Attributes) (enum *mojom.MojomEnum) {
	if p.Error() {
		return
	}
	// TODO
	return
}

func (p *Parser) parseConstDecl(attributes *mojom.Attributes) (constant *mojom.UserDefinedConstant) {
	if p.Error() {
		return
	}
	// TODO
	return
}

func (p *Parser) parseType() (mojomType mojom.Type) {
	p.consumeNextToken()
	return mojom.INT64
}

func (p *Parser) parserOrdinal() (ordinalValue int) {
	ordinalValue = -1
	if p.tryMatch(lexer.ORDINAL) {
		x, err := strconv.Atoi(p.lastPeek.Text[1:])
		if err != nil || x < 0 {
			panic("Lexer returned an ORDINAL that was not parsable as a non-negative integer.")
		}
		ordinalValue = x
	}
	return
}

///////////////// Parsing Helper Functions ////////

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
	p.lastPeek = nextToken
	return
}

// This method is similar to peekNextToken except that in the case of EOF
// it does not set the global error state but rather returns |eof| = |true|.
// This method is useful when EOF is an allowed state and you want
// to know what the extraneous token is in case it is not EOF.
func (p *Parser) checkEOF() (eof bool) {
	p.lastPeek = p.inputStream.PeekNext()
	eof = p.lastPeek.EOF()
	return
}

// Advances the cursor in the stream and returns true, or else returns
// false if the cursor is already past the end of the stream.
func (p *Parser) consumeNextToken() bool {
	return p.inputStream.ConsumeNext()
}

func (p *Parser) match(expectedKind lexer.TokenKind) bool {
	if p.Error() {
		return false
	}
	message := fmt.Sprintf("I was expecting to find %s next.", expectedKind)
	nextToken := p.peekNextToken(message)
	if p.Error() {
		return false
	}
	if nextToken.Kind != expectedKind {
		message = fmt.Sprintf("Unexpected token at %s: %s. Expecting %s.",
			nextToken.LocationString(), nextToken, expectedKind)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return false
	}
	p.consumeNextToken()
	return true
}

func (p *Parser) tryMatch(expectedKind lexer.TokenKind) bool {
	if p.checkEOF() {
		return false
	}
	if p.Error() {
		return false
	}
	nextToken := p.lastPeek
	if nextToken.Kind != expectedKind {
		return false
	}
	p.consumeNextToken()
	return true
}

func (p *Parser) matchSemicolonToken(previousToken lexer.Token) bool {
	if p.match(lexer.SEMI) {
		return true
	}
	message := fmt.Sprintf("Missing semicolon after %s at %s.",
		previousToken, previousToken.LocationString())
	p.err = parserError{E_UNEXPECTED_TOKEN, message}
	return false
}

func (p *Parser) matchSemicolon() bool {
	return p.matchSemicolonToken(p.lastPeek)
}

func (p *Parser) readText(kind lexer.TokenKind) (text string) {
	if p.Error() {
		return
	}

	if !p.match(kind) {
		return
	}
	return p.lastPeek.Text
}

func (p *Parser) readStringLiteral() (literal string) {
	return p.readText(lexer.STRING_LITERAL)
}

func (p *Parser) readName() (name string) {
	return p.readText(lexer.NAME)
}

func (p *Parser) readIdentifier() (identifier string) {
	if p.Error() {
		return
	}
	firstToken := p.peekNextToken("Expecting an identifier.")
	if firstToken.Kind != lexer.NAME {
		message := fmt.Sprintf("Unexpected token at %s: %s. Expecting an identifier.",
			firstToken.LocationString(), firstToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return
	}

	for p.tryMatch(lexer.NAME) {
		identifier += p.lastPeek.Text
		if !p.tryMatch(lexer.DOT) {
			return
		}
		identifier += "."
	}
	message := fmt.Sprintf("Invalid identifier: %s at %s. Identifier may not end with a dot.",
		identifier, firstToken.LocationString())
	p.err = parserError{E_UNEXPECTED_TOKEN, message}
	return
}

////////////////// Parse Tree /////////////////////

///// ParseNode type /////
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

func (node *ParseNode) appendChild(name string, firstToken *lexer.Token) *ParseNode {
	child := newParseNode(name)
	child.firstToken = firstToken
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
		lastPeekCopy := p.lastPeek
		childNode := p.currentNode.appendChild(name, &(lastPeekCopy))
		p.currentNode = childNode
	}
}

func (p *Parser) popNode() {
	if p.currentNode != nil {
		p.currentNode = p.currentNode.parent
	}
}
