package parser

// Type Kinds
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
// The Type interface. All of our Type types implement this.
/////////////////////////////////////////////////////////////
type Type interface {
	Kind() TypeKind
	AllowedAsMapKey() bool
	Nullable() bool
	Identical(other Type) bool
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

/////////////////////////////////////////////////////////////
// Type Reference
/////////////////////////////////////////////////////////////
type TypeReference struct {
	nullable bool

	interfaceRequest bool

	// The identifier as it appears at the reference site.
	identifier string

	resolvedType UserDefinedType
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
