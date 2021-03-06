package parser

import (
	"fmt"
	"mojom/mojom_parser/lexer"
	"mojom/mojom_parser/mojom"
	"strconv"
)

// The code in this file implements  recursive-descent, predictive parsing
// for the context-free grammar listed below.
//
// The grammar is similar to the grammar in the document "Mojom Language
// Specification", but it has been modified in order to make it LL(1). This
// is necessary in order to be able to use it to do predictive top-down
// parsing. (See Section 4.4.3 of "Compilers: Principles, Techniques and Tools"
// 2nd Edition, by Aho et al."). This requirement explains the slight awkwardness
// seen below regarding the handling of Mojom attributes. (Disclaimer: There are
// a few productions that technically render the grammar LL(n) for some small
// integer n rather than strictly LL(1). But this does not affect the parsing
// in any significant way.)
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
// ATTR_ASSIGNMENT      -> name equals name | name equals literal

// INTRFC_DECL          -> interface name lbrace INTRFC_BODY rbrace semi
// INTRFC_BODY          -> {ATTR_INTRFC_ELEMENT}
// ATTR_INTRFC_ELEMENT  -> [ATTRIBUTES] INTRFC_ELEMENT
// INTRFC_ELEMENT       -> METHOD_DECL | ENUM_DECL | CONSTANT_DECL

// METHOD_DECL          -> name [ordinal] lparen [PARAM_LIST] rparen [response lparen [PARAM_LIST] rparen] semi
// PARAM_LIST           -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL           -> [ATTRIBUTES] TYPE NAME [ordinal]

// STRUCT_DECL          -> struct name lbrace STRUCT_BODY rbrace semi
// STRUCT_BODY          -> {ATTR_STRUCT_ELEMENT}
// ATTR_STRUCT_ELEMENT  -> [ATTRIBUTES] STRUCT_ELEMENT
// STRUCT_ELEMENT       -> STRUCT_FIELD | ENUM_DECL | CONSTANT_DECL
// STRUCT_FIELD         -> TYPE name [ordinal] [equals DEFAULT_VALUE] semi
// DEFAULT_VALUE        -> APPROPRIATE_VAL_SPEC | default
// APPROPRIATE_VAL_SPEC -> VALUE_REF {{that resolves to a type that is assignment compatible to the type of the assignee}}

// UNION_DECL           -> union name lbrace UNION_BODY rbrace semi
// UNION_BODY           -> {UNION_FIELD_DECL}
// UNION_FIELD_DECL     -> [ATTRIBUTES] TYPE name [ordinal] semi

// ENUM_DECL            -> enum name lbrace ENUM_BODY rbrace semi
// ENUM_BODY            -> [ ENUN_VALUE {, ENUM_VALUE} [,] ]
// ENUM_VALUE           -> [ATTRIBUTES] name [equals ENUM_VAL_INITIALIZER]
// ENUM_VAL_INITIALIZER -> INTEGER_LITERAL | ENUM_VALUE_REF
// ENUM_VALUE_REF       -> IDENTIFIER {{that resolves to a declared enum value}}

// CONSTANT_DECL        -> const CONST_OK_TYPE name equals CONST_VALUE_REF semi
// CONST_OK_TYPE        -> SIMPLE_TYPE | string
// CONST_VALUE_REF      -> LITERAL_VALUE | CONST_VAL_REF
// CONST_VALUE_REF      -> IDENTIFIER {{that resolves to a declared constant}}

// VALUE_REF            -> USER_VALUE_REF | LITERAL_VALUE
// USER_VALUE_REF       -> CONST_VALUE_REF | ENUM_VALUE_REF
// LITERAL_VALUE        -> BOOL_LITERAL | string_literal | NUMBER_LITERAL
// BOOL_LITERAL         -> true | false
// NUMBER_LITERAL       -> [plus | minus] POS_NUM_LITERAL
// NUMBER_LITERAL       -> FLOAT_SPECIAL_IDENTIFIER
// INTEGER_LITERAL      -> [plus | minus] POS_INT_LITERAL
// POS_NUM_LITERAL      -> POS_INT_LITERAL | POS_FLOAT_LITERAL
// POS_INT_LITERAL      -> int_const_dec | int_const_hex
// POS_FLOAT_LITERAL    -> float_const

// TYPE                 -> BUILT_IN_TYPE | ARRAY_TYPE | MAP_TYPE | TYPE_REFERENCE
// BUILT_IN_TYPE        -> SIMPLE_TYPE | STRING_TYPE | HANDLE_TYPE
// SIMPLE_TYPE          -> bool | FLOAT_TYPE | INTEGER_TYPE
// FLOAT_TYPE           -> float | double
// HANDLE_TYPE          -> handle langle [HANDLE_KIND] rangle [qstn]
// HANDLE_KIND          -> message_pipe | data_pipe_consumer | data_pipe_producer | shared_buffer
// INTEGER_TYPE         -> int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64
// STRING_TYPE          -> string [qstn]
// ARRAY_TYPE           -> array langle TYPE [comma int_const_dec] rangle [qstn]
// MAP_TYPE             -> map langle MAP_KEY_TYPE comma TYPE rangle [qstn]
// MAP_KEY_TYPE         -> SIMPLE_TYPE | string | ENUM_TYPE
// TYPE_REFERENCE       -> INTERFACE_TYPE | STRUCT_TYPE | UNION_TYPE | ENUM_TYPE
// INTERFACE_TYPE       -> IDENTIFIER [amp] [qstn] {{where IDENTIFIER resolves to an interface}}
// STRUCT_TYPE          -> IDENTIFIER [qstn] {{where IDENTIFIER resolves to a struct}}
// UNION_TYPE           -> IDENTIFIER [qstn] {{where IDENTIFIER resolves to a union}}
// ENUM_TYPE            -> IDENTIFIER [qstn] {{where IDENTIFIER resolves to an interface}}

// IDENTIFIER           -> name {dot name}

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
		p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
		return false
	}

	attributes := p.parseAttributes()
	if !p.OK() {
		return false
	}
	if initialAttributes != nil {
		if attributes != nil {
			message := "File starts with two sets of attributes."
			p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
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
				p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			return false
		}
		nextToken := p.peekNextToken("")
		dupeMessage := ""
		switch nextToken.Kind {
		case lexer.INTERFACE:
			if mojomInterface, nameToken := p.parseInterfaceDecl(attributes); mojomInterface != nil {
				dupeMessage = p.duplicateNameMessage(p.mojomFile.AddInterface(mojomInterface), nameToken)
			}
		case lexer.STRUCT:
			if mojomStruct, nameToken := p.parseStructDecl(attributes); mojomStruct != nil {
				dupeMessage = p.duplicateNameMessage(p.mojomFile.AddStruct(mojomStruct), nameToken)
			}
		case lexer.UNION:
			if mojomUnion, nameToken := p.parseUnionDecl(attributes); mojomUnion != nil {
				dupeMessage = p.duplicateNameMessage(p.mojomFile.AddUnion(mojomUnion), nameToken)
			}
		case lexer.ENUM:
			if mojomEnum, nameToken := p.parseEnumDecl(attributes); mojomEnum != nil {
				dupeMessage = p.duplicateNameMessage(p.mojomFile.AddEnum(mojomEnum), nameToken)
			}
		case lexer.CONST:
			if constant, nameToken := p.parseConstDecl(attributes); constant != nil {
				dupeMessage = p.duplicateNameMessage(p.mojomFile.AddConstant(constant), nameToken)
			}
		default:
			p.unexpectedTokenError(nextToken, "interface, struct, union, enum or const")
			return false
		}

		if p.OK() && len(dupeMessage) > 0 {
			p.err = &ParseError{E_DUPLICATE_DECLARATION, dupeMessage}
			return false
		}
	}
	return p.OK()
}

// ATTRIBUTES      -> lbracket ATTR_ASSIGNMENT { comma, ATTR_ASSIGNMENT}
// ATTR_ASSIGNMENT -> name equals name | name equals literal
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
		key := p.readName()
		p.attachToken()
		if !p.OK() {
			return
		}
		if !p.match(lexer.EQUALS) {
			return
		}

		var value mojom.ConcreteValue
		if p.peekNextToken("Expecting to find an attribute value.").Kind == lexer.NAME {
			text := p.readName()
			value = mojom.MakeStringLiteralValue(text)
		} else {
			value = p.parseLiteral()
		}
		p.attachToken()

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
					nextToken, nextToken.LongLocationString())
			default:
				message = p.unexpectedTokenError(nextToken, "comma or ]")
			}
			p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
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
	if !p.OK() {
		return
	}
	switch nextToken.Kind {
	case lexer.MODULE:
		p.consumeNextToken() // Consume the MODULE token.
		break
	case lexer.IMPORT, lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return // There is no module declaration.
	default:
		p.unexpectedTokenError(nextToken, "module, import, interface, struct, union, enum or constant")
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
	if !p.OK() {
		return
	}
	for nextToken.Kind == lexer.IMPORT {
		atLeastOneImport = true
		p.pushChildNode("importStmnt")
		p.consumeNextToken() // consume the IMPORT token.

		fileName := p.readStringLiteral()
		p.attachToken()
		if !p.OK() {
			return
		}

		if !p.matchSemicolon() {
			return
		}

		p.mojomFile.AddImport(fileName)
		nextToken = p.peekNextToken("No Mojom declarations found.")
		p.popNode()
		if !p.OK() {
			return
		}
	}

	switch nextToken.Kind {
	case lexer.MODULE:
		message := "The module declaration must come before the import statements."
		p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
		return
	case lexer.INTERFACE, lexer.STRUCT, lexer.UNION, lexer.ENUM, lexer.CONST:
		return
	default:
		p.unexpectedTokenError(nextToken, "import, interface, struct, union, enum or constant")
		return
	}
}

// INTRFC_DECL  -> interface name lbrace INTRFC_BODY rbrace semi
func (p *Parser) parseInterfaceDecl(attributes *mojom.Attributes) (mojomInterface *mojom.MojomInterface, nameToken lexer.Token) {
	if !p.OK() {
		return
	}
	p.pushChildNode("interfaceDecl")
	defer p.popNode()

	if !p.match(lexer.INTERFACE) {
		return
	}

	simpleName := p.readName()
	if !p.OK() {
		return
	}
	nameToken = p.lastConsumed

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
	p.pushScope(mojomInterface.InitAsScope(p.currentScope))
	defer p.popScope()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if !p.OK() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing an interface body.")
		dupeMessage := ""
		switch nextToken.Kind {
		case lexer.NAME:
			if method := p.parseMethodDecl(attributes); p.OK() {
				mojomInterface.AddMethod(method)
				break
			}
			return false
		case lexer.ENUM:
			if mojomEnum, nameToken := p.parseEnumDecl(attributes); mojomEnum != nil {
				dupeMessage = p.duplicateNameMessage(mojomInterface.AddEnum(mojomEnum), nameToken)
			}
		case lexer.CONST:
			constant, nameToken := p.parseConstDecl(attributes)
			dupeMessage = p.duplicateNameMessage(mojomInterface.AddConstant(constant), nameToken)
		case lexer.RBRACE:
			rbraceFound = true
			if attributes != nil {
				message := "Interface body ends with extranesous attributes."
				p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			break
		default:
			p.unexpectedTokenError(nextToken, "union, enum, const or an identifier")
			return false
		}
		if p.OK() && len(dupeMessage) > 0 {
			p.err = &ParseError{E_DUPLICATE_DECLARATION, dupeMessage}
			return false
		}
	}
	if p.OK() {
		mojomInterface.ComputeMethodOrdinals()
	}
	return p.OK()
}

// METHOD_DECL -> name [ordinal] lparen [PARAM_LIST] rparen [response lparen [PARAM_LIST] rparen]semi
func (p *Parser) parseMethodDecl(attributes *mojom.Attributes) *mojom.MojomMethod {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("methodDecl")
	defer p.popNode()

	methodName := p.readName()
	if !p.OK() {
		return nil
	}

	ordinalValue := p.readOrdinal()

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
	parenBeforeSemicolon := p.lastConsumed

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
		parenBeforeSemicolon = p.lastConsumed
	}

	if !p.matchSemicolonToken(parenBeforeSemicolon) {
		return nil
	}

	mojomMethod := mojom.NewMojomMethod(methodName, ordinalValue, params, responseParams)
	return mojomMethod
}

// PARAM_LIST -> PARAM_DECL {, PARAM_DECL}
// PARAM_DECL -> [ATTRIBUTES] TYPE name [ordinal]
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
		name := p.readName()
		ordinalValue := p.readOrdinal()
		if !p.OK() {
			return
		}

		paramStruct.AddField(mojom.NewStructField(fieldType, name, ordinalValue, attributes, nil))

		nextToken = p.peekNextToken("I was parsing method parameters.")
		switch nextToken.Kind {
		case lexer.COMMA:
			p.consumeNextToken()
			continue
		case lexer.RPAREN:
			continue
		default:
			p.unexpectedTokenError(nextToken, "comma or )")
			return nil
		}
	}

	if p.OK() {
		paramStruct.ComputeFieldOrdinals()
	}
	return
}

// STRUCT_DECL   -> struct name lbrace STRUCT_BODY rbrace semi
func (p *Parser) parseStructDecl(attributes *mojom.Attributes) (mojomStruct *mojom.MojomStruct, nameToken lexer.Token) {
	if !p.OK() {
		return
	}
	p.pushChildNode("structDecl")
	defer p.popNode()

	if !p.match(lexer.STRUCT) {
		return
	}

	simpleName := p.readName()
	p.attachToken()
	if !p.OK() {
		return
	}
	nameToken = p.lastConsumed
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
	p.pushScope(mojomStruct.InitAsScope(p.currentScope))
	defer p.popScope()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if !p.OK() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing a struct body.")
		dupeMessage := ""
		switch nextToken.Kind {
		case lexer.NAME:
			mojomStruct.AddField(p.parseStructField(attributes))
		case lexer.ENUM:
			if mojomEnum, nameToken := p.parseEnumDecl(attributes); mojomEnum != nil {
				dupeMessage = p.duplicateNameMessage(mojomStruct.AddEnum(mojomEnum), nameToken)
			}
		case lexer.CONST:
			if constant, nameToken := p.parseConstDecl(attributes); constant != nil {
				dupeMessage = p.duplicateNameMessage(mojomStruct.AddConstant(constant), nameToken)
			}
		case lexer.RBRACE:
			rbraceFound = true
			if attributes != nil {
				message := "Struct body ends with extranesous attributes."
				p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			break
		default:
			p.unexpectedTokenError(nextToken, "field, enum or constant declaration")
			return false
		}
		if p.OK() && len(dupeMessage) > 0 {
			p.err = &ParseError{E_DUPLICATE_DECLARATION, dupeMessage}
			return false
		}
	}
	if p.OK() {
		mojomStruct.ComputeFieldOrdinals()
	}
	return p.OK()
}

// STRUCT_FIELD         -> TYPE name [ordinal] [equals DEFAULT_VALUE] semi
// DEFAULT_VALUE        -> APPROPRIATE_VAL_SPEC | default
// APPROPRIATE_VAL_SPEC -> VALUE_REF {{that resolves to a type that is assignment compatible to the type of the assignee}}
func (p *Parser) parseStructField(attributes *mojom.Attributes) *mojom.StructField {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("structField")
	defer p.popNode()

	fieldType := p.parseType()
	fieldName := p.readName()
	p.attachToken()
	ordinalValue := p.readOrdinal()
	var defaultValue mojom.ValueRef
	var defaultValueToken lexer.Token
	if p.tryMatch(lexer.EQUALS) {
		defaultValueToken = p.peekNextToken("Expecting a default value.")
		if defaultValueToken.Kind == lexer.DEFAULT {
			p.consumeNextToken()
			defaultValue = mojom.MakeDefaultLiteral()
		} else {
			defaultValue = p.parseValue(fieldType)
		}
	}
	if !p.matchSemicolon() {
		return nil
	}

	field := mojom.NewStructField(fieldType, fieldName, ordinalValue, attributes, defaultValue)
	if !field.ValidateDefaultValue() {
		valueString := fmt.Sprintf("%v", defaultValue)
		valueTypeString := ""
		concreteValue := defaultValue.ResolvedConcreteValue()
		if concreteValue != nil {
			valueString = fmt.Sprintf("%v", concreteValue.Value())
			valueTypeString = fmt.Sprintf(" of type %s", concreteValue.ValueType())
		}
		message := fmt.Sprintf("Illegal assignment at %s: Field %s of type %s may not be assigned the value %v%s.",
			defaultValueToken.LongLocationString(), fieldName, fieldType, valueString, valueTypeString)
		p.err = &ParseError{E_TYPE_NOT_ASSIGNMENT_COMPATIBLE, message}
		return nil
	}

	return field
}

// UNION_DECL    -> union name lbrace UNION_BODY rbrace semi
func (p *Parser) parseUnionDecl(attributes *mojom.Attributes) (union *mojom.MojomUnion, nameToken lexer.Token) {
	if !p.OK() {
		return
	}
	p.pushChildNode("unionDecl")
	defer p.popNode()

	if !p.match(lexer.UNION) {
		return
	}

	simpleName := p.readName()
	p.attachToken()
	if !p.OK() {
		return
	}
	nameToken = p.lastConsumed
	union = mojom.NewMojomUnion(simpleName, attributes)

	if !p.match(lexer.LBRACE) {
		return
	}

	if !p.parseUnionBody(union) {
		return
	}

	if !p.match(lexer.RBRACE) {
		return
	}

	if p.matchSemicolon() {
		union.ComputeFieldTags()
	}

	return
}

// UNION_BODY         -> {UNION_FIELD_DECL}
// UNION_FIELD_DECL   -> [ATTRIBUTES] TYPE name [ordinal] semi
func (p *Parser) parseUnionBody(union *mojom.MojomUnion) bool {
	if !p.OK() {
		return p.OK()
	}
	p.pushChildNode("unionBody")
	defer p.popNode()

	rbraceFound := false
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if !p.OK() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing a union body.")
		switch nextToken.Kind {
		case lexer.NAME:
			fieldType := p.parseType()
			fieldName := p.readName()
			tag := p.readOrdinal()
			if !p.matchSemicolon() {
				return false
			}
			union.AddField(fieldType, fieldName, tag, attributes)
		case lexer.RBRACE:
			rbraceFound = true
			if attributes != nil {
				message := "Enum body ends with extranesous attributes."
				p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			break
		default:
			p.unexpectedTokenError(nextToken, "either another union field or }")
			return false
		}
	}
	return p.OK()
}

// ENUM_DECL -> enum name lbrace ENUM_BODY rbrace semi
func (p *Parser) parseEnumDecl(attributes *mojom.Attributes) (enum *mojom.MojomEnum, nameToken lexer.Token) {
	if !p.OK() {
		return
	}
	p.pushChildNode("enumDecl")
	defer p.popNode()

	if !p.match(lexer.ENUM) {
		return
	}

	simpleName := p.readName()
	p.attachToken()
	if !p.OK() {
		return
	}
	nameToken = p.lastConsumed
	enum = mojom.NewMojomEnum(simpleName, attributes)

	if !p.match(lexer.LBRACE) {
		return
	}

	if !p.parseEnumBody(enum) {
		return
	}

	if !p.match(lexer.RBRACE) {
		return
	}

	p.matchSemicolon()

	return
}

// ENUM_BODY     -> [ ENUN_VALUE {, ENUM_VALUE} [,] ]
// ENUM_VALUE    -> [ATTRIBUTES] name [equals ENUM_VAL_INITIALIZER]
func (p *Parser) parseEnumBody(mojomEnum *mojom.MojomEnum) bool {
	if !p.OK() {
		return p.OK()
	}
	p.pushChildNode("enumBody")
	defer p.popNode()

	// The enum body forms a new scope in which it's enun values are defined.
	p.pushScope(mojomEnum.InitAsScope(p.currentScope))
	defer p.popScope()

	rbraceFound := false
	trailingCommaFound := false
	firstValue := true
	for attributes := p.parseAttributes(); !rbraceFound; attributes = p.parseAttributes() {
		if !p.OK() {
			return false
		}
		nextToken := p.peekNextToken("I was parsing an enum body.")
		dupeMessage := ""
		switch nextToken.Kind {
		case lexer.NAME:
			if !firstValue && !trailingCommaFound {
				message := fmt.Sprintf("Expecting a comma after %s before "+
					"the next value %s at %s. ", p.lastConsumed, nextToken,
					nextToken.LongLocationString())
				p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
				return false
			}
			firstValue = false
			name := p.readName()
			p.attachToken()
			nameToken := p.lastConsumed
			var valueRef mojom.ValueRef
			if p.tryMatch(lexer.EQUALS) {
				valueRef = p.parseEnumValueInitializer(mojomEnum)
			}
			dupeMessage = p.duplicateNameMessage(
				mojomEnum.AddEnumValue(name, valueRef, attributes), nameToken)
			trailingCommaFound = p.tryMatch(lexer.COMMA)
		case lexer.RBRACE:
			rbraceFound = true
			if attributes != nil {
				message := "Enum body ends with extranesous attributes."
				p.err = &ParseError{E_BAD_ATTRIBUTE_LOCATION, message}
			}
			break
		case lexer.COMMA:
			break
		default:
			p.unexpectedTokenError(nextToken, "either another enum value or }")
			return false
		}
		if p.OK() && len(dupeMessage) > 0 {
			p.err = &ParseError{E_DUPLICATE_DECLARATION, dupeMessage}
			return false
		}
	}
	if p.OK() {
		mojomEnum.ComputeEnumValues()
	}
	return p.OK()
}

// ENUM_VAL_INITIALIZER -> INTEGER_LITERAL | ENUM_VALUE_REF
// ENUM_VALUE_REF       -> IDENTIFIER {{that resolves to a declared enum value}}
func (p *Parser) parseEnumValueInitializer(mojoEnum *mojom.MojomEnum) mojom.ValueRef {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("enumValueInitializer")
	defer p.popNode()

	// We need to manufacture an instance of Type to act as the "assigneeType"
	// for the new ValueSpec we are creating. This is because unlike
	// other types of value assignment, an enum value initializer is not
	// preceded by a type reference for the assignee. Rather the type of
	// the assignee is implicit in the scope.
	enumType := mojom.NewResolvedUserTypeRef(mojoEnum.FullyQualifiedName(), mojoEnum)

	valueToken := p.peekNextToken("Parsing an enum value initializer type.")
	valueRef := p.parseValue(enumType)
	if valueRef == nil {
		return nil
	}
	if !valueRef.MarkUsedAsEnumValueInitializer() {
		message := fmt.Sprintf("Illegal value: %s at %s. An enum value may "+
			"only be initialized by an integer or another enum value.", valueToken,
			valueToken.LongLocationString())
		p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
		return nil
	}

	return valueRef
}

// CONSTANT_DECL     -> const CONST_OK_TYPE name equals CONST_VALUE_REF semi
// CONST_OK_TYPE     -> SIMPLE_TYPE | string
// CONST_VALUE_REF  -> LITERAL_VALUE | CONST_VAL_REF
// CONST_VALUE_REF   -> IDENTIFIER {{that resolves to a declared constant}}
func (p *Parser) parseConstDecl(attributes *mojom.Attributes) (constant *mojom.UserDefinedConstant, nameToken lexer.Token) {
	if !p.OK() {
		return
	}

	p.pushChildNode("constDecl")
	defer p.popNode()

	p.match(lexer.CONST)
	declaredTypeToken := p.peekNextToken("Parsing a type.")
	declaredType := p.parseType()
	name := p.readName()
	if !p.OK() {
		return
	}
	nameToken = p.lastConsumed
	p.match(lexer.EQUALS)
	valueToken := p.peekNextToken("Parsing a value.")
	value := p.parseValue(declaredType)
	if !p.OK() {
		return
	}

	if !declaredType.MarkUsedAsConstantType() {
		message := fmt.Sprintf("The type %s at %s is not allowed as the type "+
			"of a declared constant. Only simple types, strings and enum "+
			"types may be the types of constants.",
			declaredType, declaredTypeToken.LongLocationString())
		p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
		return
	}

	constant = mojom.NewUserDefinedConstant(name, declaredType, value, attributes)
	p.matchSemicolon()

	if !constant.ValidateValue() {
		valueString := fmt.Sprintf("%v", value)
		valueTypeString := ""
		concreteValue := value.ResolvedConcreteValue()
		if concreteValue != nil {
			valueString = fmt.Sprintf("%v", concreteValue.Value())
			valueTypeString = fmt.Sprintf(" of type %s", concreteValue.ValueType())
		}
		message := fmt.Sprintf("Illegal assignment at %s: Constant %s of type %s may not be assigned the value %v%s.",
			valueToken.LongLocationString(), name, declaredType, valueString, valueTypeString)
		p.err = &ParseError{E_TYPE_NOT_ASSIGNMENT_COMPATIBLE, message}
		return
	}

	return
}

// VALUE_REF -> USER_VALUE_REF | LITERAL_VALUE
func (p *Parser) parseValue(assigneeType mojom.TypeRef) mojom.ValueRef {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("value")
	defer p.popNode()

	nextToken := p.peekNextToken("I was parsing a value.")
	p.attachToken()
	if nextToken.Kind == lexer.NAME {
		return p.readUserValueRef(assigneeType)
	}
	literalValue := p.parseLiteral()
	p.attachToken()
	if !p.OK() || literalValue.ValueType() == nil {
		return nil
	}
	return literalValue
}

//LITERAL_VALUE  -> BOOL_LITERAL | string_literal | NUMBER_LITERAL
// BOOL_LITERAL  -> true | false
func (p *Parser) parseLiteral() mojom.LiteralValue {
	if !p.OK() {
		return mojom.LiteralValue{}
	}
	p.pushChildNode("literal")
	defer p.popNode()

	nextToken := p.peekNextToken("I was parsing a literal.")
	switch nextToken.Kind {
	case lexer.STRING_LITERAL:
		return mojom.MakeStringLiteralValue(p.readStringLiteral())
	case lexer.TRUE:
		p.consumeNextToken()
		return mojom.MakeBoolLiteralValue(true)
	case lexer.FALSE:
		p.consumeNextToken()
		return mojom.MakeBoolLiteralValue(true)
	case lexer.PLUS, lexer.MINUS, lexer.FLOAT_CONST, lexer.INT_CONST_DEC, lexer.INT_CONST_HEX:
		return p.parseNumberLiteral()

	default:
		p.unexpectedTokenError(nextToken, "a string, numeric or boolean literal")
		return mojom.LiteralValue{}
	}
}

// NUMBER_LITERAL       -> [plus | minus] POS_NUM_LITERAL
// POS_NUM_LITERAL      -> POS_INT_LITERAL | POS_FLOAT_LITERAL
func (p *Parser) parseNumberLiteral() mojom.LiteralValue {
	if !p.OK() {
		return mojom.LiteralValue{}
	}
	p.pushChildNode("numberLiteral")
	defer p.popNode()

	initialMinus := p.tryMatch(lexer.MINUS)
	initialPlus := p.tryMatch(lexer.PLUS)
	if initialMinus && initialPlus {
		p.unexpectedTokenError(p.lastConsumed, "a number")
		return mojom.LiteralValue{}
	}

	nextToken := p.peekNextToken("I was parsing a numberliteral.")
	switch nextToken.Kind {
	case lexer.INT_CONST_DEC, lexer.INT_CONST_HEX:
		value, _ := p.readPositiveIntegerLiteral(initialMinus, true, 64)
		return mojom.MakeInt64LiteralValue(value)
	case lexer.FLOAT_CONST:
		value, _ := p.readPositiveFloatLiteral(initialMinus)
		return mojom.MakeDoubleLiteralValue(value)

	default:
		p.unexpectedTokenError(nextToken, "a number")
		return mojom.LiteralValue{}
	}
}

// IDENTIFIER   -> name {dot name}
func (p *Parser) parseIdentifier() (identifier string, firstToken lexer.Token) {
	if !p.OK() {
		return
	}
	p.pushChildNode("identifier")
	defer p.popNode()

	firstToken = p.peekNextToken("Expecting an identifier.")
	if !p.OK() {
		return
	}
	if firstToken.Kind != lexer.NAME {
		p.unexpectedTokenError(firstToken, "an identifier")
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
		identifier, firstToken.LongLocationString())
	p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
	return
}

// TYPE -> BUILT_IN_TYPE | ARRAY_TYPE | MAP_TYPE | TYPE_REFERENCE
func (p *Parser) parseType() mojom.TypeRef {
	if !p.OK() {
		return nil
	}
	p.pushChildNode("type")
	defer p.popNode()

	return p.readType()
}

///////////////// Methods for parsing types and values ////////

// TYPE -> BUILT_IN_TYPE | ARRAY_TYPE | MAP_TYPE | TYPE_REFERENCE
func (p *Parser) readType() mojom.TypeRef {
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
		mojomType = p.readTypeReference()
	}

	return mojomType
}

// BUILT_IN_TYPE        -> SIMPLE_TYPE | STRING_TYPE | HANDLE_TYPE
// SIMPLE_TYPE          -> bool | FLOAT_TYPE | INTEGER_TYPE
// FLOAT_TYPE           -> float | double
// HANDLE_TYPE          -> handle langle [HANDLE_KIND] rangle [qstn]
// HANDLE_KIND          -> message_pipe | data_pipe_consumer | data_pipe_producer | shared_buffer
// INTEGER_TYPE         -> int8 | int16 | int32 | int64 | uint8 | uint16 | uint32 | uint64
// STRING_TYPE          -> string [qstn]
func (p *Parser) tryReadBuiltInType() mojom.TypeRef {
	if !p.OK() {
		return nil
	}

	typeNameToken := p.peekNextToken("I was reading a type.")
	if typeNameToken.Kind != lexer.NAME {
		return nil
	}
	typeName := typeNameToken.Text
	builtInType := mojom.BuiltInType(typeName)
	if builtInType == nil {
		return nil
	}
	p.consumeNextToken()

	// handle<*> types
	if typeName == mojom.HANDLE_PREFIX {
		if p.tryMatch(lexer.LANGLE) {
			handleType := p.readText(lexer.NAME)
			if !p.OK() {
				token := p.lastSeen
				p.unexpectedTokenError(token, "a type of handle")
				return nil
			}
			if p.match(lexer.RANGLE) {
				typeName = fmt.Sprintf("%s<%s>", typeName, handleType)
				if builtInType = mojom.BuiltInType(typeName); builtInType == nil {
					message := fmt.Sprintf("Unrecognized type of handle at %s: %s.",
						typeNameToken.LongLocationString(), typeName)
					p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
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
		if builtInType = mojom.BuiltInType(typeName + "?"); builtInType == nil {
			message := fmt.Sprintf("The type %s? at %s is invalid because the "+
				"type %s may not be made nullable.",
				typeName, typeNameToken.LongLocationString(), typeName)
			p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
			return nil
		}
	}

	return builtInType
}

// ARRAY_TYPE  -> array langle TYPE [comma int_const_dec] rangle [qstn]
func (p *Parser) tryReadArrayType() mojom.TypeRef {
	if !p.OK() {
		return nil
	}

	nextToken := p.peekNextToken("Trying to read a type.")
	if nextToken.Kind != lexer.NAME || nextToken.Text != "array" {
		return nil
	}
	p.consumeNextToken()
	if !p.match(lexer.LANGLE) {
		return nil
	}

	elementType := p.parseType()
	if !p.OK() {
		return nil
	}

	isFixedSize := p.tryMatch(lexer.COMMA)
	fixedSize := -1
	if isFixedSize {
		if size, ok := p.readPositiveIntegerLiteral(false, false, 0); ok {
			fixedSize = int(size)
		} else {
			return nil
		}

	}
	if !p.match(lexer.RANGLE) {
		return nil
	}
	nullable := p.tryMatch(lexer.QSTN)

	return mojom.NewArrayTypeRef(elementType, fixedSize, nullable)
}

// MAP_TYPE      -> map langle MAP_KEY_TYPE comma TYPE rangle [qstn]
// MAP_KEY_TYPE  -> SIMPLE_TYPE | string | ENUM_TYPE
func (p *Parser) tryReadMapType() mojom.TypeRef {
	if !p.OK() {
		return nil
	}

	nextToken := p.peekNextToken("Trying to read a type.")
	if nextToken.Kind != lexer.NAME || nextToken.Text != "map" {
		return nil
	}
	p.consumeNextToken()
	if !p.match(lexer.LANGLE) {
		return nil
	}

	keyToken := p.peekNextToken("Trying to read a type.")
	keyType := p.parseType()
	p.match(lexer.COMMA)
	valueType := p.parseType()
	if !p.match(lexer.RANGLE) {
		return nil
	}
	nullable := p.tryMatch(lexer.QSTN)

	if !keyType.MarkUsedAsMapKey() {
		message := fmt.Sprintf("The type %s at %s is not allowed as the key "+
			"type of a map. Only simple types, strings and enum types may "+
			"be map keys.",
			keyType, keyToken.LongLocationString())
		p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
		return nil
	}

	return mojom.NewMapTypeRef(keyType, valueType, nullable)
}

// TYPE_REFERENCE   -> INTERFACE_TYPE | STRUCT_TYPE | UNION_TYPE | ENUM_TYPE
// INTERFACE_TYPE   -> IDENTIFIER [amp] [qstn] {{where IDENTIFIER resolves to an interface}}
// STRUCT_TYPE      -> IDENTIFIER [qstn] {{where IDENTIFIER resolves to a struct}}
// UNION_TYPE       -> IDENTIFIER [qstn] {{where IDENTIFIER resolves to a union}}
// ENUM_TYPE        -> IDENTIFIER [qstn] {{where IDENTIFIER resolves to an interface}}
func (p *Parser) readTypeReference() mojom.TypeRef {
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
	typeReference := mojom.NewUserTypeRef(identifier, nullable,
		interfaceRequest, p.currentScope, identifierToken)
	p.mojomDescriptor.RegisterUnresolvedTypeReference(typeReference)
	return typeReference
}

// USER_VALUE_REF -> IDENTIFIER {{that resolves to an enum value or constant}}
func (p *Parser) readUserValueRef(assigneeType mojom.TypeRef) *mojom.UserValueRef {
	if !p.OK() {
		return nil
	}
	identifier, identifierToken := p.parseIdentifier()
	if !p.OK() {
		return nil
	}
	valueReference := mojom.NewUserValueRef(assigneeType, identifier,
		p.currentScope, identifierToken)
	p.mojomDescriptor.RegisterUnresolvedValueReference(valueReference)
	return valueReference
}

// POS_INT_LITERAL -> int_const_dec | int_const_hex
func (p *Parser) readPositiveIntegerLiteral(initialMinus, acceptHex bool,
	maxBits int) (int64, bool) {
	nextToken := p.peekNextToken("I was parsing an integer literal.")
	p.consumeNextToken()
	base := 10
	intText := p.lastConsumed.Text
	if initialMinus {
		intText = "-" + intText
	}
	switch nextToken.Kind {
	case lexer.INT_CONST_DEC:
		break
	case lexer.INT_CONST_HEX:
		if !acceptHex {
			message := fmt.Sprintf("Illegal value %s at %s. Only a decimal "+
				"integer literal is allowed here. Hexadecimal is not valid.",
				nextToken.LongLocationString(), nextToken)
			p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
			return 0, false
		}
		if len(intText) < 3 {
			message := fmt.Sprintf("Invalid hex integer literal"+
				" '%s' at %s.", intText, nextToken.LongLocationString())
			p.err = &ParseError{E_INTEGER_PARSE_ERROR, message}
			return 0, false
		}
		intText = intText[2:]
		base = 16

	default:
		p.unexpectedTokenError(nextToken, "a positive integer literal")
		return 0, false
	}

	intVal, err := strconv.ParseInt(intText, base, maxBits)
	if err == nil {
		return intVal, true
	}
	message := "parseIntegerLiteral error."
	switch err.(*strconv.NumError).Err {
	case strconv.ErrRange:
		message = fmt.Sprintf("Integer literal value out of range: "+
			"%s at %s.", intText, nextToken.LongLocationString())
	case strconv.ErrSyntax:
		panic(fmt.Sprintf("Lexer allowed unparsable integer literal: %s. "+
			"Kind = %s. error=%s.", nextToken.Text, nextToken.Kind, err))
	}
	p.err = &ParseError{E_INTEGER_OUT_OF_RANGE, message}
	return 0, false
}

// POS_FLOAT_LITERAL  -> float_const
func (p *Parser) readPositiveFloatLiteral(initialMinus bool) (float64, bool) {
	nextToken := p.peekNextToken("I was parsing a float literal.")
	p.match(lexer.FLOAT_CONST)
	floatText := p.lastConsumed.Text
	if initialMinus {
		floatText = "-" + floatText
	}
	floatVal, err := strconv.ParseFloat(floatText, 64)
	if err == nil {
		return floatVal, true
	}
	var message string
	switch err.(*strconv.NumError).Err {
	case strconv.ErrRange:
		message = fmt.Sprintf("Float literal value out of range: "+
			"%s at %s.", floatText, nextToken.LongLocationString())
	case strconv.ErrSyntax:
		panic(fmt.Sprintf("Lexer allowed unparsable float literal: %s. "+
			"Kind = %s. error=%s.", nextToken.Text, nextToken.Kind, err))
	}
	p.err = &ParseError{E_INTEGER_OUT_OF_RANGE, message}
	return 0, false
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
		p.err = &ParseError{E_EOF, errorMessage}
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
	message := fmt.Sprintf("Expecting %s.", expectedKind)
	nextToken := p.peekNextToken(message)
	if !p.OK() {
		return false
	}
	if nextToken.Kind != expectedKind {
		expected := fmt.Sprintf("%s", expectedKind)
		p.unexpectedTokenError(nextToken, expected)
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
		previousToken, previousToken.LongLocationString())
	p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
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

func (p *Parser) readStringLiteral() (literal string) {
	if !p.OK() {
		return
	}

	text := p.readText(lexer.STRING_LITERAL)
	if !p.OK() {
		return
	}
	length := len(text)
	if (length < 2) || (text[0] != '"') || (text[length-1] != '"') {
		panic(fmt.Sprintf("Lexer returned a string literal token whose "+
			"text was not delimited by quotation marks: '%s'.", text))
	}
	return text[1 : length-1]
}

func (p *Parser) readName() (name string) {
	if !p.OK() {
		return
	}
	name = p.readText(lexer.NAME)
	return
}

// Returns -1 if there was no specified ordinal.
func (p *Parser) readOrdinal() (ordinalValue int64) {
	if !p.OK() {
		return
	}

	ordinalValue = -1
	if p.tryMatch(lexer.ORDINAL) {
		x, err := strconv.ParseInt(p.lastConsumed.Text[1:], 10, 64)
		if err != nil || x < 0 {
			panic("Lexer returned an ORDINAL that was not parsable as a non-negative integer.")
		}
		ordinalValue = x
	}
	return
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

/////////// Utility functions

func (p *Parser) duplicateNameMessage(dupeError *mojom.DuplicateNameError,
	nameToken lexer.Token) string {
	if dupeError == nil {
		return ""
	}
	if dupeError.ExistingType() != nil {
		return fmt.Sprintf("%s:%s. Duplicate definition for name '%s'. "+
			"The fully-qualified name of this type would be the same as "+
			"an existing type definition: "+
			"%s %s in %s.", p.mojomFile.FileName,
			nameToken.ShortLocationString(), nameToken.Text,
			dupeError.ExistingType().Kind(),
			dupeError.ExistingType().FullyQualifiedName(),
			dupeError.ExistingType().Scope())

	} else {
		return fmt.Sprintf("%s:%s. Duplicate definition for name '%s'. "+
			"The fully-qualified name of this value would be the same as "+
			"an existing value definition: "+
			"%s %s in %s.", p.mojomFile.FileName,
			nameToken.ShortLocationString(), nameToken.Text,
			dupeError.ExistingValue().Kind(),
			dupeError.ExistingValue().FullyQualifiedName(),
			dupeError.ExistingValue().Scope())
	}
}

// |expected| should be a string of the grammatic form "a semicolon"
func (p *Parser) unexpectedTokenError(token lexer.Token, expected string) string {
	return p.expectedTokenError(token, "Unexpected token at %s: %s.", expected)
}

// |unexpected| should be a string of the grammatical form "Unexpected token at %s: %s."
// |expected| should be a string of the grammatic form "a semicolon"
func (p *Parser) expectedTokenError(token lexer.Token, unexpected string, expected string) string {
	message := fmt.Sprintf(" Expecting %s.", expected)
	switch token.Kind {
	case lexer.ERROR_UNKNOWN, lexer.ERROR_ILLEGAL_CHAR,
		lexer.ERROR_UNTERMINATED_STRING_LITERAL,
		lexer.ERROR_UNTERMINATED_COMMENT:
		message = fmt.Sprintf("%s at %s.", token, token.LongLocationString()) + message
	default:
		message = fmt.Sprintf(unexpected, token.LongLocationString(), token) + message
	}
	p.err = &ParseError{E_UNEXPECTED_TOKEN, message}
	return message
}
