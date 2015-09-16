package mojom

import (
	"fmt"
)

// User-Defined Type Kinds
type UserDefinedTypeKind int

const (
	STRUCT_TYPE UserDefinedTypeKind = iota
	INTERFACE_TYPE
	ENUM_TYPE
	UNION_TYPE
)

/////////////////////////////////////////////////////////////
// The UserDefinedType interface. This is implemented by
// MojomStruct, MojomInterface, MojomEnum and MojomUnion
/////////////////////////////////////////////////////////////
type UserDefinedType interface {
	Kind() UserDefinedTypeKind
	TypeKey() string
	FullyQualifiedName() string
	DeclarationData() *TypeDeclarationData
	SupportsContainedDeclarations() bool
	Identical(other UserDefinedType) bool
}

// This struct is embedded in each of MojomStruct, MojomInterface
// MojomEnum and MojomUnion
type UserDefinedTypeBase struct {
	declarationData    *TypeDeclarationData
	fullyQualifiedName string
	typeKey            string
	thisType           UserDefinedType
	file               *MojomFile
	descriptor         *MojomDescriptor
}

// This method is invoked from the constructors for the containing types:
// NewMojomInterface, NewMojomStruct, NewMojomEnum, NewMojomUnion
func (b *UserDefinedTypeBase) Init(simpleName string, thisType UserDefinedType,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) {
	b.declarationData = NewTypeDeclarationData(simpleName, attributes)
	b.thisType = thisType
	b.file = file
	b.descriptor = descriptor
}

// Generates the fully-qualified name and the type key and registers the
// type with the Mojom Descriptor.
//
// This method is invoked when the containing type is added to its container.
// This happens in MojomFile.AddInterface(), MojomFile.AddStruct()
// MojomFile.AddEnum(), MojomFile.AddUnion() and also
// UserDefinedTypeBase.AddEnum() for when enums are added to interfaces
// and structs.
//
// namespace is the fully-qualified name of the container to which the
// type is being added.
func (b *UserDefinedTypeBase) RegisterWithDescriptor(namespace string) {
	if b.descriptor == nil {
		panic("Init() must be invoked first.")
	}
	b.fullyQualifiedName = namespace + "." + b.declarationData.SimpleName
	b.typeKey = computeTypeKey(b.fullyQualifiedName)
	b.descriptor.typesByKey[b.typeKey] = b.thisType
	b.descriptor.typeKeysByFQName[b.fullyQualifiedName] = b.typeKey
}

func (b UserDefinedTypeBase) String() string {
	s := fmt.Sprintf("fullyQualifiedName: %s\n", b.fullyQualifiedName)
	s += fmt.Sprintf("typeKey: %s\n", b.typeKey)
	return s
}

func (b *UserDefinedTypeBase) TypeKey() string {
	return b.typeKey
}

func (b UserDefinedTypeBase) FullyQualifiedName() string {
	return b.fullyQualifiedName
}

func (b UserDefinedTypeBase) DeclarationData() *TypeDeclarationData {
	return b.declarationData
}

func (b UserDefinedTypeBase) Identical(other UserDefinedType) bool {
	return b.DeclarationData() == other.DeclarationData()
}

// Adds an enum to a this type, which must be an interface or struct.
func (b UserDefinedTypeBase) AddEnum(mojomEnum *MojomEnum) {
	if !b.thisType.SupportsContainedDeclarations() {
		panic(fmt.Sprintf("Type %v does not support contained declarations.", b.thisType))
	}
	mojomEnum.RegisterWithDescriptor(b.fullyQualifiedName)
	b.declarationData.ContainedDeclarations.enumKeys =
		append(b.declarationData.ContainedDeclarations.enumKeys, mojomEnum.typeKey)
}

// Adds a declared constant to a this type, which must be an interface or struct.
func (b UserDefinedTypeBase) AddDeclaredConstant(declaredConst *UserDefinedConstant) {
	if !b.thisType.SupportsContainedDeclarations() {
		panic(fmt.Sprintf("Type %v does not support contained declarations.", b.thisType))
	}
	declaredConst.RegisterWithDescriptor(b.fullyQualifiedName)
	b.declarationData.ContainedDeclarations.constantKeys =
		append(b.declarationData.ContainedDeclarations.constantKeys, declaredConst.constantKey)
}

/////////////////////////////////////////////////////////////
// Structs
/////////////////////////////////////////////////////////////
type MojomStruct struct {
	UserDefinedTypeBase

	fields []StructField
}

func NewMojomStruct(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *MojomStruct {
	mojomStruct := new(MojomStruct)
	mojomStruct.fields = make([]StructField, 0)
	mojomStruct.Init(simpleName, mojomStruct, attributes, file, descriptor)
	return mojomStruct
}

func (s *MojomStruct) AddField(field StructField) {
	s.fields = append(s.fields, field)
}

func (MojomStruct) Kind() UserDefinedTypeKind {
	return STRUCT_TYPE
}

func (s *MojomStruct) SupportsContainedDeclarations() bool {
	return true
}

// This should be invoked some time after all of the fields have been added
// to the struct.
func (s *MojomStruct) ComputeFieldOrdinals() {
	// TODO
}

// A debug string representing this struct in the case that this struct
// is being used to represent the parameters to a method.
func (s MojomStruct) ParameterString() string {
	str := ""
	for i, f := range s.fields {
		if i > 0 {
			str += ", "
		}
		attributesString := ""
		if f.Attributes != nil {
			attributesString = fmt.Sprintf("%s", f.Attributes)
		}
		ordinalString := ""
		if f.DeclaredOrdinal >= 0 {
			ordinalString = fmt.Sprintf("@%d", f.DeclaredOrdinal)
		}
		str += fmt.Sprintf("%s%s %s%s", attributesString, f.fieldType,
			f.SimpleName, ordinalString)
	}
	return str
}

type StructField struct {
	ValueDeclarationData

	fieldType    Type
	defaultValue *ConstantOccurrence
	offset       int32
}

func BuildStructField(fieldType Type, name string,
	ordinal int, attributes *Attributes, defaultValue *ConstantOccurrence) (field StructField) {
	field = StructField{fieldType: fieldType, defaultValue: defaultValue}
	field.InitValueDeclarationData(name, attributes, ordinal)
	return
}

// TODO(rudominer) Do something with this.
type StructVersion struct {
	versionNumber uint32
	numFields     uint32
	numBytes      uint32
}

/////////////////////////////////////////////////////////////
// Interfaces and Methods
/////////////////////////////////////////////////////////////

type MojomInterface struct {
	UserDefinedTypeBase

	methodsByOrdinal map[int]*MojomMethod

	methodsByName map[string]*MojomMethod
}

func NewMojomInterface(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *MojomInterface {
	mojomInterface := new(MojomInterface)
	mojomInterface.methodsByOrdinal = make(map[int]*MojomMethod)
	mojomInterface.methodsByName = make(map[string]*MojomMethod)
	mojomInterface.Init(simpleName, mojomInterface, attributes, file, descriptor)
	return mojomInterface
}

func (m *MojomInterface) AddMethod(method *MojomMethod) {
	m.methodsByName[method.SimpleName] = method
}

func (MojomInterface) Kind() UserDefinedTypeKind {
	return INTERFACE_TYPE
}

func (i *MojomInterface) SupportsContainedDeclarations() bool {
	return true
}

func (m *MojomInterface) ComputeMethodOrdinals() {
	// TODO
}

func (m *MojomInterface) String() string {
	if m == nil {
		return "nil"
	}
	s := fmt.Sprintf("\n---------interface--------------\n")
	s += fmt.Sprintf("%s", m.UserDefinedTypeBase)
	s += "     Methods\n"
	s += "     -------\n"
	for _, method := range m.methodsByName {
		s += fmt.Sprintf("     %s\n", method)
	}
	return s
}

type MojomMethod struct {
	ValueDeclarationData

	parameters *MojomStruct

	responseParameters *MojomStruct
}

func NewMojomMethod(name string, ordinalValue int, params,
	responseParams *MojomStruct) *MojomMethod {
	mojomMethod := new(MojomMethod)
	mojomMethod.InitValueDeclarationData(name, nil, ordinalValue)
	mojomMethod.parameters = params
	mojomMethod.responseParameters = responseParams
	return mojomMethod
}

func (m *MojomMethod) String() string {
	parameterString := m.parameters.ParameterString()
	responseString := ""
	if m.responseParameters != nil {
		responseString = fmt.Sprintf(" => (%s)", m.responseParameters.ParameterString())
	}
	return fmt.Sprintf("%s(%s)%s", m.SimpleName, parameterString, responseString)
}

/////////////////////////////////////////////////////////////
// Unions
/////////////////////////////////////////////////////////////
type MojomUnion struct {
	UserDefinedTypeBase

	fields []UnionField
}

func NewMojomUnion(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *MojomUnion {
	mojomUnion := new(MojomUnion)
	mojomUnion.fields = make([]UnionField, 0)
	mojomUnion.Init(simpleName, mojomUnion, attributes, file, descriptor)
	return mojomUnion
}

func (MojomUnion) Kind() UserDefinedTypeKind {
	return UNION_TYPE
}

func (u *MojomUnion) SupportsContainedDeclarations() bool {
	return false
}

type UnionField struct {
	ValueDeclarationData

	fieldType Type
	tag       uint32
}

/////////////////////////////////////////////////////////////
// Enums
/////////////////////////////////////////////////////////////
type MojomEnum struct {
	UserDefinedTypeBase

	values []EnumValue
}

func NewMojomEnum(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *MojomEnum {
	mojomEnum := new(MojomEnum)
	mojomEnum.values = make([]EnumValue, 0)
	mojomEnum.Init(simpleName, mojomEnum, attributes, file, descriptor)
	return mojomEnum
}

func (MojomEnum) Kind() UserDefinedTypeKind {
	return ENUM_TYPE
}

func (i *MojomEnum) SupportsContainedDeclarations() bool {
	return false
}

type EnumValue struct {
	ValueDeclarationData

	// The value must eventually resolve to a ConstantValue of type integer or
	// EnumConstantValue.
	value ConstantOccurrence
}

/////////////////////////////////////////////////////////////
//Declared Constants
/////////////////////////////////////////////////////////////

// This represents a Mojom constant declaration.
type UserDefinedConstant struct {
	declarationData    *TypeDeclarationData
	fullyQualifiedName string
	constantKey        string
	file               *MojomFile
	descriptor         *MojomDescriptor

	// The type must be a string, bool, float, double, or integer type.
	valueType Type

	// The value must eventually resolve to the same type as |type|.
	value ConstantOccurrence
}

func NewUserDefinedConstant(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *UserDefinedConstant {
	userDefinedConstant := new(UserDefinedConstant)
	userDefinedConstant.declarationData = NewTypeDeclarationData(simpleName, attributes)
	userDefinedConstant.file = file
	userDefinedConstant.descriptor = descriptor
	return userDefinedConstant
}

func (c *UserDefinedConstant) RegisterWithDescriptor(namespace string) {
	c.fullyQualifiedName = namespace + "." + c.declarationData.SimpleName
	c.constantKey = computeTypeKey(c.fullyQualifiedName)
	c.descriptor.constantsByKey[c.constantKey] = c
	c.descriptor.constantKeysByFQName[c.fullyQualifiedName] = c.constantKey
}

/////////////////////////////////////////////////////////////
// Declaration Data
/////////////////////////////////////////////////////////////
type TypeDeclarationData struct {
	SimpleName            string
	Attributes            *Attributes
	ContainedDeclarations *ContainedDeclarations
}

func NewTypeDeclarationData(simpleName string, attributes *Attributes) *TypeDeclarationData {
	declarationData := new(TypeDeclarationData)
	declarationData.SimpleName = simpleName
	declarationData.Attributes = attributes
	return declarationData
}

type ValueDeclarationData struct {
	SimpleName      string
	Attributes      *Attributes
	DeclaredOrdinal int
}

func (v *ValueDeclarationData) InitValueDeclarationData(simpleName string,
	attributes *Attributes, ordinal int) {
	v.SimpleName = simpleName
	v.Attributes = attributes
	if ordinal >= 0 {
		v.DeclaredOrdinal = ordinal
	}
}

type Attributes struct {
	List []MojomAttribute
}

func (a *Attributes) String() string {
	if a == nil {
		return "nil"
	}
	return fmt.Sprintf("%s", a.List)
}

func NewAttributes() *Attributes {
	attributes := new(Attributes)
	attributes.List = make([]MojomAttribute, 0)
	return attributes
}

type MojomAttribute struct {
	Key, Value string
}

type ContainedDeclarations struct {
	// The type keys of enums declared in this namespace.
	enumKeys []string

	// The the constant keys of constants declared in this namespace.
	constantKeys []string
}
