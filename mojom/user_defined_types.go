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
	SimpleName() string
	FullyQualifiedName() string
	Identical(other UserDefinedType) bool
}

// This struct is embedded in each of MojomStruct, MojomInterface
// MojomEnum and MojomUnion
type UserDefinedTypeBase struct {
	attributes         *Attributes
	thisType           UserDefinedType
	simpleName         string
	fullyQualifiedName string
	typeKey            string
	file               *MojomFile
}

// This method is invoked from the constructors for the containing types:
// NewMojomInterface, NewMojomStruct, NewMojomEnum, NewMojomUnion
func (b *UserDefinedTypeBase) Init(simpleName string, thisType UserDefinedType,
	attributes *Attributes) {
	b.thisType = thisType
	b.simpleName = simpleName
	b.attributes = attributes
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
func (b *UserDefinedTypeBase) RegisterInScope(scope *Scope, namePrefix string) {
	// Register in the given scope with the given namePrefix.
	fmt.Println("**** Adding " + namePrefix + b.SimpleName() + " to " + scope.String())
	scope.typesByName[namePrefix+b.SimpleName()] = b.thisType
	// Then propogate the registration up the scope chain prepending the
	// child scope's name to the namePrefix.
	if scope.parentScope != nil {
		b.RegisterInScope(scope.parentScope, scope.simpleName+"."+namePrefix)
	}
	b.fullyQualifiedName = scope.fullyQualifiedName + "." + b.simpleName
	b.typeKey = computeTypeKey(b.fullyQualifiedName)
	scope.file.Descriptor.typesByKey[b.typeKey] = b.thisType
	scope.file.Descriptor.typeKeysByFQName[b.fullyQualifiedName] = b.typeKey
}

func (b UserDefinedTypeBase) String() string {
	return "TODO"
}

func (b *UserDefinedTypeBase) TypeKey() string {
	return b.typeKey
}

func (b UserDefinedTypeBase) SimpleName() string {
	return b.simpleName
}

func (b UserDefinedTypeBase) FullyQualifiedName() string {
	return b.fullyQualifiedName
}

func (b UserDefinedTypeBase) Identical(other UserDefinedType) bool {
	return b.typeKey == other.TypeKey()
}

// Some user-defined types, namely interfaces and structs, may act as
// namespaced scopes for declarations of constants and enums.
type DeclarationContainer struct {
	scope *Scope
}

// Adds an enum to a this type, which must be an interface or struct.
func (c DeclarationContainer) AddEnum(mojomEnum *MojomEnum) {
	mojomEnum.RegisterInScope(c.scope, "")
}

// Adds a declared constant to a this type, which must be an interface or struct.
func (c DeclarationContainer) AddConstant(declaredConst *UserDefinedConstant) {
	declaredConst.RegisterInScope(c.scope, "")
}

/////////////////////////////////////////////////////////////
// Structs
/////////////////////////////////////////////////////////////
type MojomStruct struct {
	UserDefinedTypeBase
	DeclarationContainer

	fields []StructField
}

func NewMojomStruct(simpleName string, attributes *Attributes) *MojomStruct {
	mojomStruct := new(MojomStruct)
	mojomStruct.fields = make([]StructField, 0)
	mojomStruct.Init(simpleName, mojomStruct, attributes)
	return mojomStruct
}

func (s *MojomStruct) InitScope(parentScope *Scope) *Scope {
	file := parentScope.File()
	if file == nil {
		panic("parentScope must have file set.")
	}
	s.scope = NewScope(SCOPE_STRUCT, parentScope, s.simpleName, parentScope.File())
	return s.scope
}

func (s *MojomStruct) AddField(field StructField) {
	s.fields = append(s.fields, field)
}

func (MojomStruct) Kind() UserDefinedTypeKind {
	return STRUCT_TYPE
}

// This should be invoked some time after all of the fields have been added
// to the struct.
func (s *MojomStruct) ComputeFieldOrdinals() {
	// TODO
}

func (m MojomStruct) String() string {
	s := fmt.Sprintf("\n---------struct--------------\n")
	s += fmt.Sprintf("%s", m.UserDefinedTypeBase)
	s += "     Fields\n"
	s += "     ------\n"
	for _, field := range m.fields {
		s += fmt.Sprintf("     %s\n", field)
	}
	return s
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

func (f StructField) String() string {
	return fmt.Sprintf("%s %s", f.fieldType, f.SimpleName)
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
	DeclarationContainer

	methodsByOrdinal map[int]*MojomMethod

	methodsByName map[string]*MojomMethod
}

func NewMojomInterface(simpleName string, attributes *Attributes) *MojomInterface {
	mojomInterface := new(MojomInterface)
	mojomInterface.methodsByOrdinal = make(map[int]*MojomMethod)
	mojomInterface.methodsByName = make(map[string]*MojomMethod)
	mojomInterface.Init(simpleName, mojomInterface, attributes)
	return mojomInterface
}

func (i *MojomInterface) InitScope(parentScope *Scope) *Scope {
	file := parentScope.File()
	if file == nil {
		panic("parentScope must have file set.")
	}
	i.scope = NewScope(SCOPE_INTERFACE, parentScope,
		i.simpleName, parentScope.File())
	return i.scope
}

func (i *MojomInterface) AddMethod(method *MojomMethod) {
	i.methodsByName[method.SimpleName] = method
}

func (MojomInterface) Kind() UserDefinedTypeKind {
	return INTERFACE_TYPE
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

func NewMojomUnion(simpleName string, attributes *Attributes) *MojomUnion {
	mojomUnion := new(MojomUnion)
	mojomUnion.fields = make([]UnionField, 0)
	mojomUnion.Init(simpleName, mojomUnion, attributes)
	return mojomUnion
}

func (MojomUnion) Kind() UserDefinedTypeKind {
	return UNION_TYPE
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

func NewMojomEnum(simpleName string, attributes *Attributes) *MojomEnum {
	mojomEnum := new(MojomEnum)
	mojomEnum.values = make([]EnumValue, 0)
	mojomEnum.Init(simpleName, mojomEnum, attributes)
	return mojomEnum
}

func (MojomEnum) Kind() UserDefinedTypeKind {
	return ENUM_TYPE
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
	attributes         *Attributes
	simpleName         string
	fullyQualifiedName string
	constantKey        string

	// The type must be a string, bool, float, double, or integer type.
	valueType Type

	// The value must eventually resolve to the same type as |type|.
	value ConstantOccurrence
}

func NewUserDefinedConstant(simpleName string,
	attributes *Attributes) *UserDefinedConstant {
	userDefinedConstant := new(UserDefinedConstant)
	userDefinedConstant.simpleName = simpleName
	userDefinedConstant.attributes = attributes
	return userDefinedConstant
}

func (c *UserDefinedConstant) RegisterInScope(scope *Scope, namePrefix string) {
	// Register in the given scope with the given namePrefix.
	scope.constantsByName[namePrefix+c.simpleName] = c
	// Then propogate the registration up the scope chain prepending the
	// child scope's name to the namePrefix.
	if scope.parentScope != nil {
		c.RegisterInScope(scope.parentScope, scope.simpleName+"."+namePrefix)
	}
	c.fullyQualifiedName = scope.fullyQualifiedName + "." + c.simpleName
	c.constantKey = computeTypeKey(c.fullyQualifiedName)
	scope.file.Descriptor.constantsByKey[c.constantKey] = c
	scope.file.Descriptor.constantKeysByFQName[c.fullyQualifiedName] = c.constantKey
}

/////////////////////////////////////////////////////////////
// Declaration Data
/////////////////////////////////////////////////////////////

type ValueDeclarationData struct {
	SimpleName      string
	Attributes      *Attributes
	DeclaredOrdinal int
}

func (v *ValueDeclarationData) InitValueDeclarationData(simpleName string,
	attributes *Attributes, ordinal int) {
	v.SimpleName = simpleName
	v.Attributes = attributes
	v.DeclaredOrdinal = ordinal
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
