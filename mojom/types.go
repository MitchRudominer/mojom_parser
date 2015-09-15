package mojom

import (
	"fmt"
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

func (h HandleType) String() string {
	return "TODO(rudominer)"
}

/////////////////////////////////////////////////////////////
// Type Reference
/////////////////////////////////////////////////////////////
type TypeReference struct {
	nullable bool

	interfaceRequest bool

	// The scope where this type reference occurred. This is
	// used to resolve the identifier.
	Scope *Scope

	// The identifier as it appears at the reference site.
	identifier string

	// If this type reference was used as a constant value then resolvedTye
	// must eventually be an enum type
	usedAsConstantValue bool

	resolvedType UserDefinedType
}

type ScopeKind int

const (
	MODULE ScopeKind = iota
	INTERFACE
	STRUCT
)

type Scope struct {
	Kind               ScopeKind
	ParentScope        *Scope
	FullyQualifiedName string
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
	return "TODO(rudominer)"
}

/////////////////////////////////////////////////////////////
// Constant Occurrence
/////////////////////////////////////////////////////////////

// A constant occurrence represents an occurrence in a .mojom file of
// a constant. This may be either a constant literal or an identifier
// that refers to user-defined constant, or an identifier that refers
// to a user-defined enum value
type ConstantOccurrence struct {
	isLiteral bool

	// The value of the constant or nil if the constant is not yet resolved.
	value *ConstantValue

	// The scope where this constant reference occurred. This is
	// used to resolve the identifier.
	Scope *Scope

	identifier       string
	resolvedConstant *UserDefinedConstant
}

/////////////////////////////////////////////////////////////
// Constant Values
/////////////////////////////////////////////////////////////
///
type ConstantValue struct {
	// The Type must be simple, string, or a type references
	// whose resolvedType is an enum type. The accessor methods
	// below return the appropriate type of value.
	valueType Type

	value interface{}
}

func (cv ConstantValue) isSimpleType(simpleType SimpleType) bool {
	if cv.valueType.Kind() == SIMPLE_TYPE {
		if cv.valueType.(SimpleType) == simpleType {
			return true
		}
	}
	return false
}

func (cv ConstantValue) GetBoolValue() (value bool, success bool) {
	if success = cv.isSimpleType(BOOL); success {
		value = cv.value.(bool)
	}
	return
}

func (cv ConstantValue) GetDoubleValue() (value float64, success bool) {
	if success = cv.isSimpleType(DOUBLE); success {
		value = cv.value.(float64)
	}
	return
}

func (cv ConstantValue) GetFloatValue() (value float32, success bool) {
	if success = cv.isSimpleType(FLOAT); success {
		value = cv.value.(float32)
	}
	return
}

func (cv ConstantValue) GetInt8Value() (value int8, success bool) {
	if success = cv.isSimpleType(INT8); success {
		value = cv.value.(int8)
	}
	return
}

func (cv ConstantValue) GetInt16Value() (value int16, success bool) {
	if success = cv.isSimpleType(INT16); success {
		value = cv.value.(int16)
	}
	return
}

func (cv ConstantValue) GetInt32Value() (value int32, success bool) {
	if success = cv.isSimpleType(INT32); success {
		value = cv.value.(int32)
	}
	return
}

func (cv ConstantValue) GetInt64Value() (value int64, success bool) {
	if success = cv.isSimpleType(INT64); success {
		value = cv.value.(int64)
	}
	return
}

func (cv ConstantValue) GetIntU8Value() (value uint8, success bool) {
	if success = cv.isSimpleType(UINT8); success {
		value = cv.value.(uint8)
	}
	return
}

func (cv ConstantValue) GetUInt16Value() (value uint16, success bool) {
	if success = cv.isSimpleType(UINT16); success {
		value = cv.value.(uint16)
	}
	return
}

func (cv ConstantValue) GetUInt32Value() (value uint32, success bool) {
	if success = cv.isSimpleType(UINT32); success {
		value = cv.value.(uint32)
	}
	return
}

func (cv ConstantValue) GetUInt64Value() (value uint64, success bool) {
	if success = cv.isSimpleType(UINT64); success {
		value = cv.value.(uint64)
	}
	return
}

func (cv ConstantValue) GetEnumValue() (value EnumConstantValue, success bool) {
	if cv.valueType.Kind() == TYPE_REFERENCE {
		success = true
		value = cv.value.(EnumConstantValue)
	}
	return
}

type EnumConstantValue struct {
	// The reference must be resolved to an MojomEnum.
	enumType TypeReference

	enumValueName string

	intValue int32
}
