package mojom

import (
	"fmt"
	"mojom/mojom_parser/lexer"
)

// This file data structures and functions used to describe Mojom types,
// values type reference and value references. The difference between a type and
// a type reference is indicated by the fact that, for example, there is only
// one int32 type but a .mojom file may contain many references to that type. For
// the built-in types like int32 this distinction is not important and the same
// object will represent both the type and the type reference. But for a
// user-defined type the distinction is important. The type "struct Foo" is
// created via a mojom struct declaration and then the type is referenced in
// other places via an identifier. We will use different objects to represent
// the type and the (possibly mulitple) references to the type.
//
// This file does not contain the objects that represent user-defined types.
// Those may be found in the file user_defined_types.go. This file does
// contain the objects that represent built-in types and type references and
// user-defined type references. Type resolution refers to the process of
// mapping each user-defined type reference to its corresponding user-defined
// type.
//
// The situation is similar with values. This file contains the objects that
// represent built-in values and value references and user-defined value
// references. The objects that represent user-defined values may also
// be found in the file user_defined_types.go. Value resolution refers to the
// process of mapping each user-defined value reference to its corresponding
// user-defined value.

// The different kinds of Mojom types (and type references).
type TypeKind int

const (
	SIMPLE_TYPE TypeKind = iota
	STRING_TYPE
	ARRAY_TYPE
	MAP_TYPE
	HANDLE_TYPE
	USER_DEFINED_TYPE
)

type Stringable interface {
	String() string
}

// The literal types are the simple types plus string. These are the types
// of literals. This interface is used to represent literal types in there
// aspect as a type as opposed to a type reference.
// A LiteralType is a ConcreteType.
type LiteralType interface {
	ConcreteType
	AllowedAsEnumValueInitializer() bool
}

// The ConcreteTypes are the LiteralTypes plus enum types. These are the
// types that may be the type of a constant declaration or a default value
// assignment. Some of these types may be used as the type of an enum
// value initializer. This interface represents types as opposed to
// type references.
type ConcreteType interface {
	Stringable
	ConcreteTypeKind() TypeKind
}

// A TypeRef is a reference to any kind of type. An instance of TypeRef
// represents a particular textual occurrence of a type reference.
type TypeRef interface {
	Stringable
	TypeRefKind() TypeKind

	// The following three methods check the validity of a use of the TypeRef.
	// If it is possible to tell immediately that the use is invalid then
	// they return false. Otherwise they return true. If it is not possible to
	// determine if the use is valid before the reference is resolved, then
	// the methods record the data necessary to do the check later.

	// Can the type be used as the type of the key to a map?
	MarkUsedAsMapKey() (ok bool)

	// Can the type be used as the type of a declared constant?
	MarkUsedAsConstantType() (ok bool)

	// Can the type be used as the type of a variable to which the given
	// variable assignment is being made?
	MarkTypeCompatible(assignment VariableAssignment) (ok bool)
}

/////////////////////////////////////////////////////////////
// SimpleType
/////////////////////////////////////////////////////////////
type SimpleType int

const (
	BOOL SimpleType = iota
	DOUBLE
	FLOAT
	INT8
	INT16
	INT32
	INT64
	UINT8
	UINT16
	UINT32
	UINT64
)

var allSimpleTypes = []SimpleType{BOOL, DOUBLE, FLOAT, INT8, INT16, INT32, INT64, UINT8, UINT16, UINT32, UINT64}

// A SimpleType is a LiteralType
func (SimpleType) LiteralTypeKind() TypeKind {
	return SIMPLE_TYPE
}

func (t SimpleType) AllowedAsEnumValueInitializer() bool {
	if t == BOOL || t == DOUBLE || t == FLOAT {
		return false
	}
	return true
}

// A SimpleType is a ConcreteType
func (SimpleType) ConcreteTypeKind() TypeKind {
	return SIMPLE_TYPE
}

// A SimpleType is a TypeRef.
func (SimpleType) TypeRefKind() TypeKind {
	return SIMPLE_TYPE

}

// From interface TypeRef
func (SimpleType) Nullable() bool {
	return false
}

// From interface TypeRef
func (SimpleType) MarkUsedAsMapKey() bool {
	return true
}

// From interface TypeRef
func (SimpleType) MarkUsedAsConstantType() bool {
	return true
}

// From interface TypeRef
func (t SimpleType) MarkTypeCompatible(assignment VariableAssignment) bool {
	if assignment.assignedValue.IsDefault() {
		// We don't support assigning "default" to a SimpleType variable.
		return false
	}
	switch assignment.assignedValue.LiteralValueType() {
	case StringLiteralType:
		// Not valid to assign a string to a SimpleType variable.
		return false
	case BOOL:
		return t == BOOL
	case DOUBLE, FLOAT:
		return t == DOUBLE || t == FLOAT
	case INT8, INT16, INT32, INT64, UINT8, UINT16, UINT32, UINT64:
		return t == INT8 || t == INT16 || t == INT32 || t == INT64 ||
			t == UINT8 || t == UINT16 || t == UINT32 || t == UINT64
	}
	// TODO(rudominer) Refine numeric type compatability checking.
	return true
}

func (t SimpleType) String() string {
	switch t {
	case BOOL:
		return "bool"
	case DOUBLE:
		return "double"
	case FLOAT:
		return "float"
	case INT8:
		return "int8"
	case INT16:
		return "int16"
	case INT32:
		return "int32"
	case INT64:
		return "int64"
	case UINT8:
		return "uint8"
	case UINT16:
		return "uint16"
	case UINT32:
		return "uint32"
	case UINT64:
		return "uint64"
	default:
		panic(fmt.Sprintf("unexpected type: &d", t))
	}

}

/////////////////////////////////////////////////////////////
//String Type
/////////////////////////////////////////////////////////////
type StringType struct {
	// When used as a type reference indicates whether or not
	// the reference is nullable. When used as a type this
	// value is ignored.
	nullable bool
}

// A global singleton representing the unique LiteralType string.
var StringLiteralType LiteralType = StringType{}

// A StringType is a LiteralType
func (StringType) LiteralTypeKind() TypeKind {
	return STRING_TYPE
}

// A StringType is a ConcreteType
func (StringType) ConcreteTypeKind() TypeKind {
	return STRING_TYPE
}

// From interface ConcreteType
func (StringType) AllowedAsEnumValueInitializer() bool {
	return false
}

// A StringType is a TypeRef
func (StringType) TypeRefKind() TypeKind {
	return STRING_TYPE
}

// From interface TypeRef
func (StringType) MarkUsedAsMapKey() bool {
	return true
}

// From interface TypeRef
func (StringType) MarkUsedAsConstantType() bool {
	return true
}

// From interface TypeRef
func (StringType) MarkTypeCompatible(assignment VariableAssignment) (ok bool) {
	v := assignment.assignedValue
	return v.LiteralValueType() == StringLiteralType || v.IsDefault()
}

// From interface TypeRef
func (s StringType) String() string {
	nullableSpecifier := ""
	if s.nullable {
		nullableSpecifier = "?"
	}
	return fmt.Sprintf("string%s", nullableSpecifier)
}

/////////////////////////////////////////////////////////////
// Handle Type
/////////////////////////////////////////////////////////////

type HandleKind int

const (
	H_UNSPECIFIED HandleKind = iota
	H_MESSAGE_PIPE
	H_DATA_PIPE_CONSUMER
	H_DATA_PIPE_PRODUCER
	H_SHARED_BUFFER
)

var allHandleKinds = []HandleKind{H_UNSPECIFIED, H_MESSAGE_PIPE, H_DATA_PIPE_CONSUMER, H_DATA_PIPE_PRODUCER, H_SHARED_BUFFER}

// This struct is only ever used to represent type references, never types.
type HandleType struct {
	nullable bool

	kind HandleKind
}

func (HandleType) TypeRefKind() TypeKind {
	return HANDLE_TYPE
}

func (HandleType) MarkUsedAsMapKey() bool {
	return false
}

func (HandleType) MarkUsedAsConstantType() bool {
	return false
}

// From interface TypeRef
func (HandleType) MarkTypeCompatible(assignment VariableAssignment) bool {
	return assignment.assignedValue.IsDefault()
}

const HANDLE_PREFIX = "handle"

func (h HandleType) String() string {
	suffix := ""
	switch h.kind {
	case H_UNSPECIFIED:
		break
	case H_MESSAGE_PIPE:
		suffix = "<message_pipe>"
	case H_DATA_PIPE_CONSUMER:
		suffix = "<data_pipe_consumer>"
	case H_DATA_PIPE_PRODUCER:
		suffix = "<data_pipe_producer>"
	case H_SHARED_BUFFER:
		suffix = "<shared_buffer>"
	default:
		panic(fmt.Sprintf("Unrecognized handle kind %d", h.kind))
	}
	nullable := ""
	if h.nullable {
		nullable = "?"
	}
	return fmt.Sprintf("%s%s%s", HANDLE_PREFIX, suffix, nullable)
}

/////////////////////////////////////////////////////////////
//
// Built-In Types
//
// The built-in types are defined to be those for which a type reference is
// immediatley fully-resolved. These are the simple types, string,
// the handle types and their nullable variants.
//
// We make a map of all built-in types by name.  The values of the
// map are of type TypeRef because this map is only used in the
// context of type references, not types.
/////////////////////////////////////////////////////////////

var allBuiltInTypes []TypeRef
var builtInTypeMap map[string]TypeRef

// Initialize allBuiltInTypes and TypeRefMap
func init() {
	allBuiltInTypes = make([]TypeRef, len(allSimpleTypes)+len(allHandleKinds)*2+2)
	// Add the simple types
	for i, t := range allSimpleTypes {
		allBuiltInTypes[i] = t
	}

	// Add the string types
	i := len(allSimpleTypes)
	allBuiltInTypes[i] = StringType{false}
	i++
	allBuiltInTypes[i] = StringType{true}
	i++

	// Add the handle types
	for _, kind := range allHandleKinds {
		allBuiltInTypes[i] = HandleType{false, kind}
		i++
		allBuiltInTypes[i] = HandleType{true, kind}
		i++
	}

	builtInTypeMap = make(map[string]TypeRef, len(allBuiltInTypes))
	for _, t := range allBuiltInTypes {
		builtInTypeMap[t.String()] = t
	}
}

func BuiltInType(name string) TypeRef {
	return builtInTypeMap[name]
}

/////////////////////////////////////////////////////////////
// ArrayTypeRef
/////////////////////////////////////////////////////////////

// This struct is only ever used to represent type references, never types.
type ArrayTypeRef struct {
	nullable bool

	// If fixed_length < 0 then the array does not have a fixed length;
	fixedLength int

	elementType TypeRef
}

func NewArrayTypeRef(elementType TypeRef, fixedSize int, nullable bool) *ArrayTypeRef {
	return &ArrayTypeRef{nullable, fixedSize, elementType}
}

// An ArrayTypeRef is a TypeRef
func (ArrayTypeRef) TypeRefKind() TypeKind {
	return ARRAY_TYPE
}

func (ArrayTypeRef) MarkUsedAsMapKey() bool {
	return false
}

func (ArrayTypeRef) MarkUsedAsConstantType() bool {
	return false
}

// From interface TypeRef
func (ArrayTypeRef) MarkTypeCompatible(assignment VariableAssignment) bool {
	return assignment.assignedValue.IsDefault()
}

func (a ArrayTypeRef) Nullable() bool {
	return a.nullable
}

func (a ArrayTypeRef) String() string {
	fixedLengthSpecifier := ""
	if a.fixedLength > 0 {
		fixedLengthSpecifier = fmt.Sprint(" ,%d", a.fixedLength)
	}
	nullableSpecifier := ""
	if a.nullable {
		nullableSpecifier = "?"
	}
	return fmt.Sprintf("array<%s%s>%s", a.elementType, fixedLengthSpecifier, nullableSpecifier)
}

/////////////////////////////////////////////////////////////
// MapTypeRef
/////////////////////////////////////////////////////////////

// This struct is only ever used to represent type references, never types.
type MapTypeRef struct {
	nullable bool

	/// The key_type must be a simple type, a string or an enum type.
	keyType   TypeRef
	valueType TypeRef
}

func NewMapTypeRef(keyType TypeRef, valueType TypeRef, nullable bool) *MapTypeRef {
	return &MapTypeRef{nullable, keyType, valueType}
}

// A MapTypeRef is a TypeRef
func (MapTypeRef) TypeRefKind() TypeKind {
	return ARRAY_TYPE
}

func (MapTypeRef) MarkUsedAsMapKey() bool {
	return false
}

func (MapTypeRef) MarkUsedAsConstantType() bool {
	return false
}

// From interface TypeRef
func (MapTypeRef) MarkTypeCompatible(assignment VariableAssignment) bool {
	return assignment.assignedValue.IsDefault()
}

func (m MapTypeRef) Nullable() bool {
	return m.nullable
}

func (m MapTypeRef) String() string {
	nullableSpecifier := ""
	if m.nullable {
		nullableSpecifier = "?"
	}
	return fmt.Sprintf("map<%s, %s>%s", m.keyType, m.valueType, nullableSpecifier)
}

/////////////////////////////////////////////////////////////
// UserTypeRef
//
// A UserTypeRef represents an identifier that refers to
// a user-defined type: an interface, struct, union or enum.
/////////////////////////////////////////////////////////////
type UserTypeRef struct {
	nullable bool

	interfaceRequest bool

	// The scope where this type reference occurred. This is
	// used to resolve the identifier.
	scope *Scope

	// The type identifier as it appears at the reference site.
	identifier string

	token lexer.Token

	usedAsMapKey       bool
	usedAsConstantType bool
	variableAssignment *VariableAssignment

	resolvedType UserDefinedType
}

func NewUserTypeRef(identifier string, nullable bool,
	interfaceRequest bool, scope *Scope, token lexer.Token) *UserTypeRef {
	return &UserTypeRef{identifier: identifier,
		nullable: nullable, interfaceRequest: interfaceRequest,
		scope: scope, token: token}
}

func NewResolvedUserTypeRef(identifier string, resolvedType UserDefinedType) *UserTypeRef {
	return &UserTypeRef{identifier: identifier, resolvedType: resolvedType}
}

// A UserTypeRef is a TypeRef
func (UserTypeRef) TypeRefKind() TypeKind {
	return USER_DEFINED_TYPE
}

func (t *UserTypeRef) ResolvedType() UserDefinedType {
	return t.resolvedType
}

func (t *UserTypeRef) MarkUsedAsMapKey() bool {
	t.usedAsMapKey = true
	return true
}

func (t *UserTypeRef) MarkUsedAsConstantType() bool {
	t.usedAsConstantType = true
	return true
}

func (t *UserTypeRef) MarkTypeCompatible(assignment VariableAssignment) bool {
	t.variableAssignment = &assignment
	return true
}

func (t *UserTypeRef) Nullable() bool {
	return t.nullable
}

func (ref *UserTypeRef) validateAfterResolution() error {
	if ref.resolvedType.Kind() != ENUM_TYPE {
		// A type ref has resolved to a non-enum type. Make sure it is not
		// being used as either a map key or a constant declaration. Also
		// make sure that a literal was not assigned to it.
		if ref.usedAsMapKey {
			return fmt.Errorf("The type %s at %s is not allowed as the key "+
				"type of a map. Only simple types, strings and enum types may "+
				"be map keys.",
				ref.identifier, ref.token.LongLocationString())
		}
		if ref.usedAsConstantType {
			return fmt.Errorf("The type %s at %s is not allowed as the type "+
				"of a declared constant. Only simple types, strings and enum "+
				"types may be the types of constants.",
				ref.identifier, ref.token.LongLocationString())
		}
	}
	if ref.variableAssignment != nil && !ref.resolvedType.IsAssignmentCompatibleWith(ref.variableAssignment.assignedValue) {
		return fmt.Errorf("Type validation error\n"+
			"%s:%s: Illegal assignment: %s %s of type %s may not be assigned the value %v of type %s.",
			ref.scope.file.FileName, ref.token.ShortLocationString(),
			ref.variableAssignment.kind, ref.variableAssignment.variableName,
			ref.identifier, ref.variableAssignment.assignedValue,
			ref.variableAssignment.assignedValue.LiteralValueType())
	}
	return nil
}

func (t *UserTypeRef) String() string {
	interfaceRequest := ""
	if t.interfaceRequest {
		interfaceRequest = "&"
	}
	nullable := ""
	if t.nullable {
		nullable = "?"
	}
	resolvedKey := ""
	if t.resolvedType != nil {
		resolvedKey = t.resolvedType.TypeKey()
	}
	return fmt.Sprintf("(%s)%s%s%s", resolvedKey, t.identifier, interfaceRequest, nullable)
}

func (t *UserTypeRef) LongString() string {
	return fmt.Sprintf("%s %s:%s. (In %s.)", t.identifier,
		t.scope.file.FileName, t.token.ShortLocationString(), t.scope)
}

/////////////////////////////////////////////////////////////
// ValueRef
/////////////////////////////////////////////////////////////

// A ValueRef represents an occurrence in the .mojom file of a
// reference to a value. These occur as the default values of fields,
// as the values of declared constants, and as the explicitly assigned value of
// an enum value.  A ValueRef is either a LiteralValue or a UserValueRef.
type ValueRef interface {
	ResolvedConcreteValue() ConcreteValue
	MarkUsedAsEnumValueInitializer() bool
}

// A reference to a user-defined value. That is, a reference to an EnumValue or
// a UserDefinedConstant.
type UserValueRef struct {

	// The scope in which the reference occurs. This is necessary in order
	// to resolve the reference.
	scope *Scope

	// The first token that the parser associates with the value reference.
	token lexer.Token

	// The identifier as it appears in the text.
	identifier string

	usedAsEnumValueInitializer bool

	// A value specification always occurs in the context of some
	// assignment. This may be the assignment of a default value
	// to a field, the assignment of a value to a declared constant,
	// or the assignment of a value to an enum value. In all cases we
	// know at the site of the assignment what the declared type of
	// the assignee is and we record that here. After the UserValueRef
	// has been resolved it we will check that the type of |resolvedValue|
	// is compatible with |assigneeType|.
	assigneeType TypeRef

	// The user-defined constant or enum value that the reference resolves to.
	resolvedDeclaredValue UserDefinedValue

	// The concrete value that the reference resolves to.
	// If |resolvedDeclaredValue| is an EnumValue then that is already
	// a ConcreteValue and |resolvedConcreteValue| == |resolvedDeclaredValue|.
	// But if |resolvedDeclaredValue| is a UserDefinedConstant then,
	// since those are not considered ConcreteValues,
	// |resolvedConcreteValue| is that constan't resolved value.
	resolvedConcreteValue ConcreteValue
}

func (v UserValueRef) ResolvedConcreteValue() ConcreteValue {
	return v.resolvedConcreteValue
}

func (v *UserValueRef) MarkUsedAsEnumValueInitializer() bool {
	v.usedAsEnumValueInitializer = true
	return true
}

func (v *UserValueRef) validateAfterResolution() error {
	// TODO(rudominer) Implement UserValueRef.validateAfterResolution()
	// This method should check the following:
	// - If |usedAsEnumValueInitializer| is true then |resolvedDeclaredValue|
	// must be an EnumValue
	// - The |resolvedConcreteValue| must have a type that is assignment
	// compatible with |assigneeType|.
	return nil
}

func (v *UserValueRef) String() string {
	resolvedKey := ""
	if v.resolvedDeclaredValue != nil {
		resolvedKey = v.resolvedDeclaredValue.ValueKey()
	}
	return fmt.Sprintf("(%s)%s", resolvedKey, v.identifier)
}

func (v *UserValueRef) LongString() string {
	return fmt.Sprintf("%s %s:%s. (In %s.)", v.identifier,
		v.scope.file.FileName, v.token.ShortLocationString(), v.scope)
}

func NewUserValueRef(assigneeType TypeRef, identifier string, scope *Scope,
	token lexer.Token) *UserValueRef {
	valueReference := new(UserValueRef)
	valueReference.assigneeType = assigneeType
	valueReference.scope = scope
	valueReference.token = token
	valueReference.identifier = identifier

	return valueReference
}

/////////////////////////////////////////////////////////////
// Concrete Values
/////////////////////////////////////////////////////////////

// A ConcreteValue is a LiteralValue or an EnumValue
type ConcreteValue interface {
	ValueType() ConcreteType
	Value() interface{}
}

/////////////////////////////////////////////////////////////
// Literal Values
/////////////////////////////////////////////////////////////

// A LiteralValue represents a string, number or boolean literal.
// The LiteralValue struct implements both ValueRef and ConcreteValue.
// This reflects the fact that a literal value is already resolved.
type LiteralValue struct {
	// The Type must be simple or string
	valueType LiteralType

	value interface{}

	// Does this LiteralValue represent the pseudo value "default"
	isDefault bool
}

func MakeStringLiteralValue(text string) LiteralValue {
	return LiteralValue{StringLiteralType, text, false}
}

func MakeBoolLiteralValue(value bool) LiteralValue {
	return LiteralValue{BOOL, value, false}
}

func MakeInt64LiteralValue(value int64) LiteralValue {
	return LiteralValue{INT64, value, false}
}

func MakeDoubleLiteralValue(value float64) LiteralValue {
	return LiteralValue{DOUBLE, value, false}
}

func MakeDefaultLiteral() LiteralValue {
	return LiteralValue{StringLiteralType, "default", true}
}

func (lv LiteralValue) String() string {
	if lv.isDefault {
		return "default"
	}
	switch lv.valueType.ConcreteTypeKind() {
	case STRING_TYPE:
		return fmt.Sprintf("\"%v\"", lv.value)
	default:
		return fmt.Sprintf("%v", lv.value)
	}
}

func (lv LiteralValue) MarkUsedAsEnumValueInitializer() bool {
	return lv.valueType.AllowedAsEnumValueInitializer()
}

func (lv LiteralValue) LiteralValueType() LiteralType {
	return lv.valueType
}

// A LiteralValue is a ConcreteValue.
func (lv LiteralValue) ValueType() ConcreteType {
	return lv.valueType
}
func (v LiteralValue) Value() interface{} {
	return v.value
}

// A LiteralValue is also a ValueRef and is its own
// ResolvedValue.
func (v LiteralValue) ResolvedConcreteValue() ConcreteValue {
	return v
}

func (v LiteralValue) IsDefault() bool {
	return v.isDefault
}

// VariableAssignment represents an assignment of a literal value to a variable.
// It is used to help track of whether or not the assignment is valid and if not
// to issue an appropriate error message.
type VariableAssignmentKind int

const (
	DEFAULT_STRUCT_FIELD VariableAssignmentKind = iota
	CONSTANT_DECLARATION
)

func (k VariableAssignmentKind) String() string {
	switch k {
	case DEFAULT_STRUCT_FIELD:
		return "field"
	case CONSTANT_DECLARATION:
		return "const"
	default:
		panic(fmt.Sprintf("Unknown VariableAssignmentKind %d", k))
	}
}

type VariableAssignment struct {
	assignedValue LiteralValue
	variableName  string
	kind          VariableAssignmentKind
}
