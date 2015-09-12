package mojom

type MojomDescriptor struct {
	typesByKey     map[string]*UserDefinedType
	constantsByKey map[string]*UserDefinedType

	interfaceByName map[string]*MojomInterface
	structsByName   map[string]*MojomStruct
	unionsByName    map[string]*MojomUnion
	enumsByName     map[string]*MojomEnum
	constantsByName map[string]*DeclaredConstant

	mojomFiles []MojomFile
}

type MojomFile struct {
	Descriptor *MojomDescriptor

	// The module name is (derived from) the file name of the corresponding
	// .mojom file. It is the unique identifier for this module within the
	// MojomFileGraph
	ModuleName string

	// The namespace is the identifier declared via the "module" declaration
	// in the .mojom file.
	moduleNamespace string

	// Attributes declared in the Mojom file at the module level.
	attributes MojomAttributes

	// The list of other MojomFiles imported by this one. The elements
	// of the array are the |module_name|s and the associated module may
	// be retrieved from the  MojomFileGraph.
	imports []*MojomFile

	interfaces []*MojomInterface
	structs    []*MojomStruct
	unions     []*MojomUnion
	enums      []*MojomEnum
	constatns  []*DeclaredConstant
}

// User-Defined Type Kinds
type UserDefinedTypeKind int

const (
	STRUCT_TYPE UserDefinedTypeKind = iota
	INTERFACE_TYPE
	ENUM_TYPE
	UNION_TYPE
)

/////////////////////////////////////////////////////////////
// The UserDefinedType interface. All of our user-defined types
// implement this.
/////////////////////////////////////////////////////////////
type UserDefinedType interface {
	Kind() UserDefinedTypeKind
	GetTypeKey() string
	DeclarationData() *DeclarationData
	Identical(other UserDefinedType) bool
}

type UserDefinedTypeBase struct {
	declarationData *DeclarationData
	typeKey         string
}

func (b UserDefinedTypeBase) GetTypeKey() string {
	return b.typeKey
}

func (b UserDefinedTypeBase) SetDeclarationData(d *DeclarationData) {
	b.declarationData = d
}

func (b UserDefinedTypeBase) DeclarationData() *DeclarationData {
	return b.declarationData
}

func (b UserDefinedTypeBase) Identical(other UserDefinedType) bool {
	return b.DeclarationData() == other.DeclarationData()
}

/////////////////////////////////////////////////////////////
// Structs
/////////////////////////////////////////////////////////////
type MojomStruct struct {
	UserDefinedTypeBase

	fields []StructField
}

type StructField struct {
	DeclarationData

	fieldType    Type
	defaultValue ConstantOccurrence
	offset       int32
}

type StructVersion struct {
	versionNumber uint32
	numFields     uint32
	numBytes      uint32
}

func (MojomStruct) Kind() UserDefinedTypeKind {
	return STRUCT_TYPE
}

/////////////////////////////////////////////////////////////
// Interfaces and Methods
/////////////////////////////////////////////////////////////

type MojomInterface struct {
	UserDefinedTypeBase

	methodsByOrdinal map[int]*MojomMethod

	methodsByName map[string]*MojomMethod
}

type MojomMethod struct {
	declarationData *DeclarationData

	parameters MojomStruct

	hasResponse        bool
	responseParameters MojomStruct
}

func (MojomInterface) Kind() UserDefinedTypeKind {
	return INTERFACE_TYPE
}

func (i MojomInterface) GetTypeKey() string {
	return i.typeKey
}

func (i MojomInterface) DeclarationData() *DeclarationData {
	return i.declarationData
}

func (i MojomInterface) Identical(other UserDefinedType) bool {
	return i.DeclarationData() == other.DeclarationData()
}

/////////////////////////////////////////////////////////////
// Unions
/////////////////////////////////////////////////////////////
type MojomUnion struct {
	UserDefinedTypeBase

	fields []UnionField
}

type UnionField struct {
	DeclarationData

	fieldType Type
	tag       uint32
}

func (MojomUnion) Kind() UserDefinedTypeKind {
	return UNION_TYPE
}

/////////////////////////////////////////////////////////////
// Enums
/////////////////////////////////////////////////////////////
type MojomEnum struct {
	UserDefinedTypeBase

	value []EnumValue
}

type EnumValue struct {
	DeclarationData

	// The value must eventually resolve to a ConstantValue of type integer or
	// EnumConstantValue.
	value ConstantOccurrence
}

func (MojomEnum) Kind() UserDefinedTypeKind {
	return ENUM_TYPE
}

/////////////////////////////////////////////////////////////
//Declared Constants
/////////////////////////////////////////////////////////////

// This represents a Mojom constant declaration.
type DeclaredConstant struct {
	DeclarationData

	// The type must be a string, bool, float, double, or integer type.
	valueType Type

	// The value must eventually resolve to the same type as |type|.
	value ConstantOccurrence
}

/////////////////////////////////////////////////////////////
// Constant Values
/////////////////////////////////////////////////////////////

type ConstantOccurrence struct {
	value ConstantValue

	identifier  string
	constantKey string
}

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

/////////////////////////////////////////////////////////////
// Declaration Data
/////////////////////////////////////////////////////////////
type DeclarationData struct {
	Name                  string
	Attributes            MojomAttributes
	containedDeclarations *ContainedDeclarations
}

type MojomAttributes []MojomAttribute

type MojomAttribute struct {
	key, value string
}

type ContainedDeclarations struct {
	// The type keys of enums declared in this namespace.
	enumKeys []string

	// The the constant keys of constants declared in this namespace.
	constantKeys []string
}
