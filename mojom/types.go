package mojom

import (
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"strings"
)

// The different kinds of Mojom types. We divide the types into five categories:
// simple, string, compound, handle, and user-defined.
type TypeKind int

const (
	SIMPLE_TYPE TypeKind = iota
	STRING_TYPE
	ARRAY_TYPE
	MAP_TYPE
	HANDLE_TYPE
	TYPE_REFERENCE
)

/////////////////////////////////////////////////////////////
// The Type interface. All of our structs that represent types
// implement this interface.
/////////////////////////////////////////////////////////////
type Type interface {
	Kind() TypeKind
	AllowedAsMapKey() bool
	Nullable() bool
	Identical(other Type) bool
	String() string
}

/////////////////////////////////////////////////////////////
// The built-in types are defined to be those with a statically
// defined identifier. These are the simple types, string,
// the handle types and their nullable variants.
/////////////////////////////////////////////////////////////

var allBuiltInTypes []Type
var BuiltInTypeMap map[string]Type

// Initialize allBuiltInTypes and BuiltInTypeMap
func init() {
	allBuiltInTypes = make([]Type, len(allSimpleTypes)+12)
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
	allBuiltInTypes[i] = HandleType{false, H_UNSPECIFIED}
	i++
	allBuiltInTypes[i] = HandleType{true, H_UNSPECIFIED}
	i++
	allBuiltInTypes[i] = HandleType{false, H_MESSAGE_PIPE}
	i++
	allBuiltInTypes[i] = HandleType{true, H_MESSAGE_PIPE}
	i++
	allBuiltInTypes[i] = HandleType{false, H_DATA_PIPE_CONSUMER}
	i++
	allBuiltInTypes[i] = HandleType{true, H_DATA_PIPE_CONSUMER}
	i++
	allBuiltInTypes[i] = HandleType{false, H_DATA_PIPE_PRODUCER}
	i++
	allBuiltInTypes[i] = HandleType{true, H_DATA_PIPE_PRODUCER}
	i++
	allBuiltInTypes[i] = HandleType{false, H_SHARED_BUFFER}
	i++
	allBuiltInTypes[i] = HandleType{true, H_SHARED_BUFFER}

	// Construct BuiltInTypeMap
	BuiltInTypeMap = make(map[string]Type, len(allBuiltInTypes))
	for _, t := range allBuiltInTypes {
		BuiltInTypeMap[t.String()] = t
	}
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

func (SimpleType) Kind() TypeKind {
	return SIMPLE_TYPE
}

func (SimpleType) AllowedAsMapKey() bool {
	return true
}

func (SimpleType) Nullable() bool {
	return false
}

func (t SimpleType) Identical(other Type) bool {
	if other.Kind() != SIMPLE_TYPE {
		return false
	}
	return t == other.(SimpleType)
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
	nullable bool
}

func (StringType) Kind() TypeKind {
	return STRING_TYPE
}

func (StringType) AllowedAsMapKey() bool {
	return true
}

func (s StringType) Nullable() bool {
	return s.nullable
}

func (s StringType) Identical(other Type) bool {
	return other.Kind() == STRING_TYPE
}

func (s StringType) String() string {
	nullableSpecifier := ""
	if s.nullable {
		nullableSpecifier = "?"
	}
	return fmt.Sprintf("string%s", nullableSpecifier)
}

/////////////////////////////////////////////////////////////
// Array Type
/////////////////////////////////////////////////////////////
type ArrayType struct {
	nullable bool

	// If fixed_length < 0 then the array does not have a fixed length;
	fixedLength int

	elementType Type
}

func (ArrayType) Kind() TypeKind {
	return ARRAY_TYPE
}

func (ArrayType) AllowedAsMapKey() bool {
	return false
}

func (a ArrayType) Nullable() bool {
	return a.nullable
}

func (a ArrayType) Identical(other Type) bool {
	if other.Kind() != ARRAY_TYPE {
		return false
	}
	otherArrayType := other.(ArrayType)
	if !(a.elementType.Identical(otherArrayType.elementType)) {
		return false
	}
	return a.fixedLength == otherArrayType.fixedLength
}

func (a ArrayType) String() string {
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
// Map Type
/////////////////////////////////////////////////////////////
type MapType struct {
	nullable bool

	/// The key_type must be a non-reference type or a string.
	keyType   Type
	valueType Type
}

func (MapType) Kind() TypeKind {
	return ARRAY_TYPE
}

func (MapType) AllowedAsMapKey() bool {
	return false
}

func (m MapType) Nullable() bool {
	return m.nullable
}

func (m MapType) Identical(other Type) bool {
	if other.Kind() != MAP_TYPE {
		return false
	}
	otherMapType := other.(MapType)
	if !(m.keyType.Identical(otherMapType.keyType)) {
		return false
	}
	if !(m.valueType.Identical(otherMapType.valueType)) {
		return false
	}
	return true
}

func (m MapType) String() string {
	nullableSpecifier := ""
	if m.nullable {
		nullableSpecifier = "?"
	}
	return fmt.Sprintf("map<%s%s>%s", m.keyType, m.valueType, nullableSpecifier)
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

type HandleType struct {
	nullable bool

	kind HandleKind
}

func (HandleType) Kind() TypeKind {
	return HANDLE_TYPE
}

func (HandleType) AllowedAsMapKey() bool {
	return false
}

func (h HandleType) Nullable() bool {
	return h.nullable
}

func (h HandleType) Identical(other Type) bool {
	if other.Kind() != HANDLE_TYPE {
		return false
	}
	otherHandleType := other.(HandleType)
	return h.kind == otherHandleType.kind
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
// Type Reference
/////////////////////////////////////////////////////////////
type TypeReference struct {
	nullable bool

	interfaceRequest bool

	// The scope where this type reference occurred. This is
	// used to resolve the identifier.
	scope *Scope

	// The type identifier as it appears at the reference site.
	identifier string

	token lexer.Token

	resolvedType UserDefinedType
}

func NewTypeReference(identifier string, nullable bool,
	interfaceRequest bool, scope *Scope, token lexer.Token) *TypeReference {
	return &TypeReference{identifier: identifier,
		nullable: nullable, interfaceRequest: interfaceRequest,
		scope: scope, token: token}
}

func (TypeReference) Kind() TypeKind {
	return TYPE_REFERENCE
}

func (TypeReference) AllowedAsMapKey() bool {
	return false
}

func (t TypeReference) Nullable() bool {
	return t.nullable
}

func (t TypeReference) Identical(other Type) bool {
	if other.Kind() != TYPE_REFERENCE {
		return false
	}
	otherTypeReference := other.(TypeReference)
	if t.interfaceRequest != otherTypeReference.interfaceRequest {
		return false
	}
	if t.resolvedType == nil || otherTypeReference.resolvedType == nil {
		return false
	}
	return t.resolvedType.Identical(otherTypeReference.resolvedType)
}

func (t TypeReference) String() string {
	interfaceRequest := ""
	if t.interfaceRequest {
		interfaceRequest = "&"
	}
	nullable := ""
	if t.nullable {
		nullable = "?"
	}
	return fmt.Sprintf("%s%s%s", t.identifier, interfaceRequest, nullable)
}

func (t TypeReference) LongString() string {
	return fmt.Sprintf("%s %s:%s. (In %s.)", t.identifier,
		t.scope.file.FileName, t.token.ShortLocationString(), t.scope)
}

/////////////////////////////////////////////////////////////
// ValueSpec
/////////////////////////////////////////////////////////////

// A ValueSpec represents an occurrence in the .mojom file of a
// specification of a value. These occur as the default values of fields,
// as the values of declared constants, and as the value of an enum value.
// A value spec is either a literal number or string or else a reference
// that must eventually resolve to an enum value or a UserDefinedConstant
type ValueSpec interface {
	ResolvedValue() *ConcreteValue
}

// Both LiteralValue and ValueReference include ValueSpecBase.
type ValueSpecBase struct {
	// A value specification always occurs in the context of some
	// assignment. This may be the assignment of a default value
	// to a field, the assignment of a value to a declared constant,
	// or the assignment of a value to an enum value. In all cases we
	// know at the site of the assignment what the declared type of
	// the assignee is and we record that here. After the ValueSpec
	// has been resolved it will also have a |valueType| and that
	// must be compatible with the |assigneeType|.
	assigneeType Type

	// The actual type of the value that the ValueSpec resolves to.
	// If the ValueSpec is a literal then we can determine this type
	// immediately when the ValueSpec is parsed. Otherwise we have to
	// wait until resolution to know the value type.
	valueType Type

	// The concrete value that the ValueSpec resolves to. If the ValueSpec
	// is a literal value then we can determine this immediately when the
	// value spec is parsed. Otherwise we have to wait until resolution
	// to know the value.
	resolvedValue *ConcreteValue
}

func (v *ValueSpecBase) LongString() string {
	return "TODO"
}

func (v *ValueSpecBase) ResolvedValue() *ConcreteValue {
	return v.resolvedValue
}

// A literal number or string value.
type LiteralValue struct {
	ValueSpecBase
}

func NewIntegerLiteralValue(assigneeType Type, intVal int64) *LiteralValue {
	int64Type := BuiltInTypeMap["int64"]
	literalValue := new(LiteralValue)
	literalValue.assigneeType = assigneeType
	literalValue.valueType = int64Type
	literalValue.resolvedValue = &ConcreteValue{valueType: int64Type, value: intVal}
	return literalValue
}

// A reference to a value. That is, a reference to an enum value or
// a reference to a user-defined constant.
type ValueReference struct {
	ValueSpecBase

	// The scope in which the reference occurs. This is necessary in order
	// to resolve the reference.
	scope *Scope
	token lexer.Token

	identifier string

	// The name of the constant or enum value. This is only the final
	// segment of the dotted identifier. The previous segments are a
	// reference to the container for the constant or the enum type and
	// are captured by valueType
	valueName string

	// This is the previous segments of the dotted identifier, not including
	// the final segment. In the case this reference turns out to refer to
	// an enum value, this string will refer to the enum type. In case this
	// reference turns out to refer to a constant declaration, this string will
	// refer to the scope of that
	valueScopeName string

	// In case the identifier resolves to the name of a constant, this is
	// a pointer to the declaration of that constant.
	resolvedConstant *UserDefinedConstant
}

func NewValueReference(assigneeType Type, identifier string, scope *Scope,
	token lexer.Token) *ValueReference {
	valueReference := new(ValueReference)
	valueReference.valueType = assigneeType
	valueReference.scope = scope
	valueReference.token = token
	valueReference.identifier = identifier

	splitIdentifier := strings.Split(identifier, ".")
	numSegments := len(splitIdentifier)

	valueReference.valueName = splitIdentifier[numSegments-1]

	valueReference.valueScopeName = ""
	if numSegments > 1 {
		valueReference.valueScopeName = strings.Join(splitIdentifier[0:numSegments-1], ".")
	}

	return valueReference
}

// We need to manufacture an instance of Type to act as the "assigneeType"
// for the new ValueReference we are creating. This is because unlike
// other types of value assignment, an enum value initializer is not
// preceded by a type reference for the assignee. Rather the type of
// the assignee is implicit in the scope.
func TypeForEnumValueInitializer(mojoEnum *MojomEnum) *TypeReference {
	enumType := NewTypeReference(mojoEnum.fullyQualifiedName, false, false,
		mojoEnum.scope, lexer.Token{})
	enumType.resolvedType = mojoEnum
	return enumType
}

/////////////////////////////////////////////////////////////
// Constant Values
/////////////////////////////////////////////////////////////
///
type ConcreteValue struct {
	// The Type must be simple, string, or a type references
	// whose resolvedType is an enum type. The accessor methods
	// below return the appropriate type of value.
	valueType Type

	value interface{}
}

func (cv ConcreteValue) String() string {
	return fmt.Sprintf("%v", cv.value)
}

func makeBuiltinConcreteValue(typeName string, value interface{}) ConcreteValue {
	return ConcreteValue{BuiltInTypeMap[typeName], value}
}

func MakeStringConcreteValue(text string) ConcreteValue {
	return makeBuiltinConcreteValue("string", text)
}

func MakeBoolConcreteValue(value bool) ConcreteValue {
	return makeBuiltinConcreteValue("bool", value)
}

func MakeInt64ConcreteValue(value int64) ConcreteValue {
	return makeBuiltinConcreteValue("int64", value)
}

func (cv ConcreteValue) isSimpleType(simpleType SimpleType) bool {
	if cv.valueType.Kind() == SIMPLE_TYPE {
		if cv.valueType.(SimpleType) == simpleType {
			return true
		}
	}
	return false
}

func (cv ConcreteValue) GetBoolValue() (value bool, success bool) {
	if success = cv.isSimpleType(BOOL); success {
		value = cv.value.(bool)
	}
	return
}

func (cv ConcreteValue) GetDoubleValue() (value float64, success bool) {
	if success = cv.isSimpleType(DOUBLE); success {
		value = cv.value.(float64)
	}
	return
}

func (cv ConcreteValue) GetFloatValue() (value float32, success bool) {
	if success = cv.isSimpleType(FLOAT); success {
		value = cv.value.(float32)
	}
	return
}

func (cv ConcreteValue) GetInt8Value() (value int8, success bool) {
	if success = cv.isSimpleType(INT8); success {
		value = cv.value.(int8)
	}
	return
}

func (cv ConcreteValue) GetInt16Value() (value int16, success bool) {
	if success = cv.isSimpleType(INT16); success {
		value = cv.value.(int16)
	}
	return
}

func (cv ConcreteValue) GetInt32Value() (value int32, success bool) {
	if success = cv.isSimpleType(INT32); success {
		value = cv.value.(int32)
	}
	return
}

func (cv ConcreteValue) GetInt64Value() (value int64, success bool) {
	if success = cv.isSimpleType(INT64); success {
		value = cv.value.(int64)
	}
	return
}

func (cv ConcreteValue) GetIntU8Value() (value uint8, success bool) {
	if success = cv.isSimpleType(UINT8); success {
		value = cv.value.(uint8)
	}
	return
}

func (cv ConcreteValue) GetUInt16Value() (value uint16, success bool) {
	if success = cv.isSimpleType(UINT16); success {
		value = cv.value.(uint16)
	}
	return
}

func (cv ConcreteValue) GetUInt32Value() (value uint32, success bool) {
	if success = cv.isSimpleType(UINT32); success {
		value = cv.value.(uint32)
	}
	return
}

func (cv ConcreteValue) GetUInt64Value() (value uint64, success bool) {
	if success = cv.isSimpleType(UINT64); success {
		value = cv.value.(uint64)
	}
	return
}

func (cv ConcreteValue) GetEnumValue() (value EnumValue, success bool) {
	if cv.valueType.Kind() == TYPE_REFERENCE {
		success = true
		value = cv.value.(EnumValue)
	}
	return
}
