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
// production.
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

// METHOD_DECL          -> name [ORDINAL] lparen [PARAM_LIST] rparen [response lparen [PARAM_LIST] rparen] semi
// PARAM_LIST           -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL           -> [ATTRIBUTES] TYPE NAME [ORDINAL]

// STRUCT_DECL          -> struct name lbrace STRUCT_BODY rbrace semi
// STRUCT_BODY          -> {ATTR_STRUCT_ELEMENT}
// ATTR_STRUCT_ELEMENT  -> [ATTRIBUTES] STRUCT_ELEMENT
// STRUCT_ELEMENT       -> STRUCT_FIELD | ENUM_DECL | CONSTANT_DECL
// STRUCT_FIELD         -> TYPE name [ORDINAL] [equals DEFAULT_VALUE] semi

// DEFAULT_VALUE        -> CONSTANT_VALUE | ENUM_VALUE_IDENTIFIER | default

////////////////////////////////////////////////////////////////////////////
// parseX() methods follow.
////////////////////////////////////////////////////////////////////////////

// Note about the input and output of the parseX() methods: The method
// Parser.OK() indicates whether or not there has been a parsing error.
// All of the methods start with the check:
//	if !p.OK()
//		return
//	}
// and sometimes perform this check midway through their logic too. To make
// this more concise some of the methods return the value of Parser.OK() so that
// a caller can check the value without an additional 'if' clause. If a parseX()
// method has no other value it needs to return it always returns the value of
// Parser.OK().
//
// Many of the methods construct a Mojom object and return it. For example
// parseInterfaceDecl() returns a MojomInterface. If the method is responsible
// for parsing a block containing arbitrarily many elements then instead the
// method is passed a container that it fills up. For example
// parseInterfaceBody() is passed a MojomInterface that it fills up and it
// returns the bool Parser.OK(). If a Mojom object can take optional attributes
// then the attributes are parsed first via parseAttributes() and then the
// attributes are passed into the method that parses the object. For example
// parseInterfaceDecl() takes the parameter attributes *mojom.Attributes.

// ATTR_MOJOM_FILE  -> [ATTRIBUTES] MOJOM_FILE
// MOJOM_FILE       -> MODULE_DECL {IMPORT_STMNT} {ATTR_MOJOM_DECL}
// MOJOM_FILE       -> IMPORT_STMNT {IMPORT_STMNT} {ATTR_MOJOM_DECL}
// MOJOM_FILE       -> MOJOM_DECL {ATTR_MOJOM_DECL}
//
// Returns Parser.OK()
func (p *Parser) parseMojomFile() bool {
	if !p.OK() {
		return false
	}
	p.pushRootNode("MojomFile")
	defer p.popNode()

	initialAttributes := p.parseAttributes()

	moduleIdentifier := p.parseModuleDecl()
	if !p.OK() {
		return false
	}

	// Set up the root scope
	p.pushScope(p.mojomFile.SetModuleNamespace(moduleIdentifier))
	defer p.popScope()

	if len(moduleIdentifier) > 0 {
		p.mojomFile.Attributes = initialAttributes
		initialAttributes = nil
	}

	importStatementsExist := p.parseImportStatements()
	if !p.OK() {
		return false
	}

	if len(moduleIdentifier) > 0 && importStatementsExist && initialAttributes != nil {
		message := "Attributes are not allowed before an import statement."
		p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
		return false
	}

	attributes := p.parseAttributes()
	if !p.OK() {
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
		if !p.OK() {
			return false
		}
		if p.checkEOF() {
			if attributes != nil {
				message := "File ends with extranesous attributes."
				p.err = parserError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			return false
		}
		nextToken := p.peekNextToken("")
		switch nextToken.Kind {
		case lexer.INTERFACE:
			p.mojomFile.AddInterface(p.parseInterfaceDecl(attributes))
		case lexer.STRUCT:
			p.mojomFile.AddStruct(p.parseStructDecl(attributes))
		case lexer.UNION:
			p.mojomFile.AddUnion(p.parseUnionDecl(attributes))
		case lexer.ENUM:
			p.mojomFile.AddEnum(p.parseEnumDecl(attributes))
		case lexer.CONST:
			p.mojomFile.AddConstant(p.parseConstDecl(attributes))
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
	if !p.OK() {
		return
	}

	if !p.tryMatch(lexer.LBRACKET) {
		// There is no attributes section here
		return
	}

	p.pushChildNode("attributes")
	defer p.popNode()

	attributes = mojom.NewAttributes()

	nextToken := p.lastConsumed
	for nextToken.Kind != lexer.RBRACKET {
		key := p.parseName()
		if !p.OK() {
			return
		}
		if !p.match(lexer.EQUALS) {
			return
		}
		value := p.parseName()
		if !p.OK() {
			return
		}
		attributes.List = append(attributes.List, mojom.MojomAttribute{key, value})

		nextToken = p.peekNextToken("I was reading an attributes section.")
		if !p.OK() {
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
//
// If there is a module declaration then the identifier is returned. Otherwise
// the empty string is returned. Check p.OK() for errors.
func (p *Parser) parseModuleDecl() (moduleIdentifier string) {
	if !p.OK() {
		return
	}
	nextToken := p.peekNextToken("No Mojom declarations found.")
	switch nextToken.Kind {
	case lexer.MODULE:
		p.consumeNextToken() // Consume the MODULE token.
		break
	case lexer.IMPORT, lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return // There is no module declaration.
	default:
		message := fmt.Sprintf("Unexpected token at %s: %s. "+
			"Expecting module, import, interface, struct, union, enum or constant.",
			nextToken.LocationString(), nextToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return
	}

	p.pushChildNode("moduleDecl")
	defer p.popNode()

	moduleIdentifier, _ = p.parseIdentifier()
	p.matchSemicolon()
	return
}

// IMPORT_STMNT  -> import string_literal
func (p *Parser) parseImportStatements() (atLeastOneImport bool) {
	if !p.OK() {
		return
	}

	nextToken := p.peekNextToken("No Mojom declarations found.")
	for nextToken.Kind == lexer.IMPORT {
		atLeastOneImport = true
		p.pushChildNode("importStmnt")
		p.consumeNextToken() // consume the IMPORT token.

		fileName := p.parseStringLiteral()
		if !p.OK() {
			return
		}

		if !p.matchSemicolon() {
			return
		}

		p.mojomFile.AddImport(fileName[1 : len(fileName)-1])
		nextToken = p.peekNextToken("No Mojom declarations found.")
		p.popNode()
		if !p.OK() {
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
	if !p.OK() {
		return
	}
	p.pushChildNode("interfaceDecl")
	defer p.popNode()

	if !p.match(lexer.INTERFACE) {
		return
	}

	simpleName := p.parseName()
	if !p.OK() {
		return
	}

	mojomInterface = mojom.NewMojomInterface(simpleName, attributes)

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
	if !p.OK() {
		return p.OK()
	}
	p.pushChildNode("interfaceBody")
	defer p.popNode()

	// The interface body forms a new scope.
	p.pushScope(mojomInterface.InitScope(p.currentScope))
	defer p.popScope()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if !p.OK() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing an interface body.")
		switch nextToken.Kind {
		case lexer.NAME:
			if method := p.parseMethodDecl(attributes); p.OK() {
				mojomInterface.AddMethod(method)
				break
			}
			return false
		case lexer.ENUM:
			if enum := p.parseEnumDecl(attributes); p.OK() {
				mojomInterface.AddEnum(enum)
				break
			}
			return false
		case lexer.CONST:
			mojomInterface.AddConstant(p.parseConstDecl(attributes))
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
	if !p.OK() {
		return nil
	}
	p.pushChildNode("methodDecl")
	defer p.popNode()

	methodName := p.parseName()
	if !p.OK() {
		return nil
	}

	ordinalValue := p.parseOrdinal()

	if !p.match(lexer.LPAREN) {
		return nil
	}

	params := p.parseParamList()
	if !p.OK() {
		return nil
	}

	if !p.match(lexer.RPAREN) {
		return nil
	}
	rParenBeforeSemicolon := p.lastConsumed

	// Check for a response message
	var responseParams *mojom.MojomStruct = nil
	if p.tryMatch(lexer.RESPONSE) {
		if !p.match(lexer.LPAREN) {
			return nil
		}

		responseParams = p.parseParamList()
		if !p.OK() {
			return nil
		}

		if !p.match(lexer.RPAREN) {
			return nil
		}
		rParenBeforeSemicolon = p.lastConsumed
	}

	if !p.matchSemicolonToken(rParenBeforeSemicolon) {
		return nil
	}

	mojomMethod := mojom.NewMojomMethod(methodName, ordinalValue, params, responseParams)
	return mojomMethod
}

// PARAM_LIST -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL -> [ATTRIBUTES] TYPE name [ORDINAL]
//
// Returns a MojomStruct containing the list of parameters. This may
// be nil in case of an early error. Check Parser.OK().
func (p *Parser) parseParamList() (paramStruct *mojom.MojomStruct) {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("paramList")
	defer p.popNode()

	paramStruct = mojom.NewMojomStruct("SyntheticParamStruct", nil)
	nextToken := p.peekNextToken("I was parsing method parameters.")
	for nextToken.Kind != lexer.RPAREN {

		attributes := p.parseAttributes()
		fieldType := p.parseType()
		if !p.OK() {
			return
		}
		name := p.parseName()
		ordinalValue := p.parseOrdinal()
		if !p.OK() {
			return
		}

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
	if !p.OK() {
		return
	}
	p.pushChildNode("structDecl")
	defer p.popNode()

	if !p.match(lexer.STRUCT) {
		return
	}

	simpleName := p.parseName()
	if !p.OK() {
		return
	}
	mojomStruct = mojom.NewMojomStruct(simpleName, attributes)

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
	if !p.OK() {
		return p.OK()
	}
	p.pushChildNode("structBody")
	defer p.popNode()

	// The struct body forms a new scope.
	p.pushScope(mojomStruct.InitScope(p.currentScope))
	defer p.popScope()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if !p.OK() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing a struct body.")
		switch nextToken.Kind {
		case lexer.NAME:
			mojomStruct.AddField(p.parseStructField(attributes))
		case lexer.ENUM:
			mojomStruct.AddEnum(p.parseEnumDecl(attributes))
		case lexer.CONST:
			mojomStruct.AddConstant(p.parseConstDecl(attributes))
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

// STRUCT_FIELD -> TYPE name [ORDINAL] [equals DEFAULT_VALUE] semi
func (p *Parser) parseStructField(attributes *mojom.Attributes) (structField mojom.StructField) {
	if !p.OK() {
		return
	}
	p.pushChildNode("structField")
	defer p.popNode()

	fieldType := p.parseType()
	fieldName := p.parseName()
	ordinalValue := p.parseOrdinal()
	if !p.matchSemicolon() {
		return
	}

	structField = mojom.BuildStructField(fieldType, fieldName, ordinalValue, attributes, nil)

	return
}

func (p *Parser) parseUnionDecl(attributes *mojom.Attributes) (union *mojom.MojomUnion) {
	if !p.OK() {
		return
	}
	// TODO
	return
}

func (p *Parser) parseEnumDecl(attributes *mojom.Attributes) (enum *mojom.MojomEnum) {
	if !p.OK() {
		return
	}
	// TODO
	return
}

func (p *Parser) parseConstDecl(attributes *mojom.Attributes) (constant *mojom.UserDefinedConstant) {
	if !p.OK() {
		return
	}
	// TODO
	return
}

func (p *Parser) parseOrdinal() (ordinalValue int) {
	if !p.OK() {
		return
	}

	ordinalValue = -1
	if p.tryMatch(lexer.ORDINAL) {
		x, err := strconv.Atoi(p.lastConsumed.Text[1:])
		if err != nil || x < 0 {
			panic("Lexer returned an ORDINAL that was not parsable as a non-negative integer.")
		}
		ordinalValue = x
		p.pushChildNode("structField")
		p.popNode()
	}
	return
}

func (p *Parser) parseType() mojom.Type {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("type")
	defer p.popNode()

	return p.readType()
}

func (p *Parser) parseStringLiteral() (literal string) {
	if !p.OK() {
		return
	}
	p.pushChildNode("stringLiteral")
	defer p.popNode()

	return p.readText(lexer.STRING_LITERAL)
}

func (p *Parser) parseIdentifier() (identifier string, firstToken lexer.Token) {
	if !p.OK() {
		return
	}
	p.pushChildNode("identifier")
	defer p.popNode()

	firstToken = p.peekNextToken("Expecting an identifier.")
	if firstToken.Kind != lexer.NAME {
		message := fmt.Sprintf("Unexpected token at %s: %s. Expecting an identifier.",
			firstToken.LocationString(), firstToken)
		p.err = parserError{E_UNEXPECTED_TOKEN, message}
		return
	}

	for p.tryMatch(lexer.NAME) {
		identifier += p.lastConsumed.Text
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

func (p *Parser) parseName() (name string) {
	if !p.OK() {
		return
	}
	name = p.readText(lexer.NAME)
	p.pushChildNode("name")
	p.popNode()
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
	p.lastSeen = nextToken
	return
}

// This method is similar to peekNextToken except that in the case of EOF
// it does not set the global error state but rather returns |eof| = |true|.
// This method is useful when EOF is an allowed state and you want
// to know what the extraneous token is in case it is not EOF.
func (p *Parser) checkEOF() (eof bool) {
	p.lastSeen = p.inputStream.PeekNext()
	eof = p.lastSeen.EOF()
	return
}

// Sets p.lastConsumed to the value of the next available token in the
// stream and then advances the stream cursor. If the cursor is already
// past the end of the stream then it sets p.lastConsumed to the EOF
// token.
func (p *Parser) consumeNextToken() {
	p.lastConsumed = p.inputStream.PeekNext()
	p.inputStream.ConsumeNext()
}

func (p *Parser) match(expectedKind lexer.TokenKind) bool {
	if !p.OK() {
		return false
	}
	message := fmt.Sprintf("I was expecting to find %s next.", expectedKind)
	nextToken := p.peekNextToken(message)
	if !p.OK() {
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
	if !p.OK() {
		return false
	}
	nextToken := p.peekNextToken("")
	if nextToken.Kind != expectedKind {
		return false
	}
	p.consumeNextToken()
	return true
}

func (p *Parser) matchSemicolonToken(previousToken lexer.Token) bool {
	if !p.OK() {
		return false
	}
	if p.match(lexer.SEMI) {
		return true
	}
	message := fmt.Sprintf("Missing semicolon after %s at %s.",
		previousToken, previousToken.LocationString())
	p.err = parserError{E_UNEXPECTED_TOKEN, message}
	return false
}

func (p *Parser) matchSemicolon() bool {
	return p.matchSemicolonToken(p.lastConsumed)
}

func (p *Parser) readText(kind lexer.TokenKind) (text string) {
	if !p.OK() {
		return
	}

	if !p.match(kind) {
		return
	}
	return p.lastConsumed.Text
}

///////////////// Methods for parsing typess ////////
func (p *Parser) readType() mojom.Type {
	if !p.OK() {
		return nil
	}

	mojomType := p.tryReadBuiltInType()
	if mojomType == nil {
		mojomType = p.tryReadArrayType()
	}
	if mojomType == nil {
		mojomType = p.tryReadMapType()
	}
	if mojomType == nil {
		mojomType = p.readUserDefinedType()
	}

	return mojomType
}

func (p *Parser) tryReadBuiltInType() mojom.Type {
	if !p.OK() {
		return nil
	}

	typeNameToken := p.peekNextToken("I was reading a type.")
	if typeNameToken.Kind != lexer.NAME {
		return nil
	}
	typeName := typeNameToken.Text
	builtInType, ok := mojom.BuiltInTypeMap[typeName]
	if !ok {
		return nil
	}
	p.consumeNextToken()

	// handle<*> types
	if typeName == mojom.HANDLE_PREFIX {
		if p.tryMatch(lexer.LANGLE) {
			handleType := p.readText(lexer.NAME)
			if !p.OK() {
				token := p.lastSeen
				message := fmt.Sprintf("Unexpected token at %s: %s. Expecting a type of handle.",
					token.LocationString(), token)
				p.err = parserError{E_UNEXPECTED_TOKEN, message}
				return nil
			}
			if p.match(lexer.RANGLE) {
				typeName = fmt.Sprintf("%s<%s>", typeName, handleType)
				if builtInType, ok = mojom.BuiltInTypeMap[typeName]; !ok {
					message := fmt.Sprintf("Unrecognized type of handle at %s: %s.",
						typeNameToken.LocationString(), typeName)
					p.err = parserError{E_UNEXPECTED_TOKEN, message}
					return nil
				}
			}
		}
		if !p.OK() {
			return nil
		}
	}

	// Check for nullable marker
	if p.tryMatch(lexer.QSTN) {
		if builtInType, ok = mojom.BuiltInTypeMap[typeName+"?"]; !ok {
			message := fmt.Sprintf("The type %s? at %s is invalid because the "+
				"type %s may not be made nullable.",
				typeName, typeNameToken.LocationString(), typeName)
			p.err = parserError{E_UNEXPECTED_TOKEN, message}
			return nil
		}
	}

	return builtInType
}

func (p *Parser) tryReadArrayType() mojom.Type {
	if !p.OK() {
		return nil
	}
	return nil
}

func (p *Parser) tryReadMapType() mojom.Type {
	if !p.OK() {
		return nil
	}
	return nil
}

func (p *Parser) readUserDefinedType() mojom.Type {
	if !p.OK() {
		return nil
	}
	identifier, identifierToken := p.parseIdentifier()
	if !p.OK() {
		return nil
	}
	// Note we must check for '&' before we check for '?'.
	// The other order is invalid and if it occurrs the extraneous
	// '&' will be detected later.
	interfaceRequest := p.tryMatch(lexer.AMP)
	nullable := p.tryMatch(lexer.QSTN)
	if !p.OK() {
		return nil
	}
	userDefinedType := mojom.NewTypeReference(identifier, nullable,
		interfaceRequest, p.currentScope, identifierToken)
	p.mojomDescriptor.RegisterUnresolvedTypeReference(userDefinedType)
	return userDefinedType
}

////////////////// Scope Stack /////////////////////
///
func (p *Parser) pushScope(scope *mojom.Scope) {
	if p.currentScope == nil {
		p.currentScope = scope
	} else {
		if scope.Parent() != p.currentScope {
			panic("Can only push child of current scope.")
		}
		p.currentScope = scope
	}
}

func (p *Parser) popScope() {
	if p.currentScope != nil {
		p.currentScope = p.currentScope.Parent()
	}
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
		tokenCopy := p.lastSeen
		childNode := p.currentNode.appendChild(name, &(tokenCopy))
		p.currentNode = childNode
	}
}

func (p *Parser) popNode() {
	if p.currentNode != nil {
		p.currentNode = p.currentNode.parent
	}
}
