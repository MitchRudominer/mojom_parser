package parser

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

func (s MojomStruct) GetTypeKey() string {
	return s.typeKey
}

func (s MojomStruct) DeclarationData() *DeclarationData {
	return s.declarationData
}

func (s MojomStruct) Identical(other UserDefinedType) bool {
	return s.DeclarationData() == other.DeclarationData()
}

/////////////////////////////////////////////////////////////
// Interfaces
/////////////////////////////////////////////////////////////

type MojomInterface struct {
	UserDefinedTypeBase

	// The keys are the method ordinals
	methods map[int]MojomMethod
	// TODO(rudominer) Do we also want a map of methods by name?
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

type DeclarationData struct {
	name       string
	attributes MojomAttributes
}
type ConstantOccurrence struct{}
type MojomAttributes struct{}
