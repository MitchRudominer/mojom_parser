package mojom

import (
	"errors"
	"fmt"
	"github.com/rudominer/mojom_parser/lexer"
	"math"
)

// User-Defined Type Kinds
type UserDefinedTypeKind int

const (
	STRUCT_TYPE UserDefinedTypeKind = iota
	INTERFACE_TYPE
	ENUM_TYPE
	UNION_TYPE
)

func (k UserDefinedTypeKind) String() string {
	switch k {
	case STRUCT_TYPE:
		return "struct"
	case INTERFACE_TYPE:
		return "interface"
	case ENUM_TYPE:
		return "enum"
	case UNION_TYPE:
		return "union"
	default:
		panic(fmt.Sprintf("Unknown UserDefinedTypeKind: %d", k))
	}
}

/////////////////////////////////////////////////////////////
// The UserDefinedType interface. This is implemented by
// MojomStruct, MojomInterface, MojomEnum and MojomUnion
/////////////////////////////////////////////////////////////
type UserDefinedType interface {
	SimpleName() string
	FullyQualifiedName() string
	Kind() UserDefinedTypeKind
	TypeKey() string
	Scope() *Scope
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
	scope              *Scope
}

// This method is invoked from the constructors for the containing types:
// NewMojomInterface, NewMojomStruct, NewMojomEnum, NewMojomUnion
func (b *UserDefinedTypeBase) Init(simpleName string, thisType UserDefinedType,
	attributes *Attributes) {
	b.thisType = thisType
	b.simpleName = simpleName
	b.attributes = attributes
}

func (b *UserDefinedTypeBase) TypeKind() TypeKind {
	return USER_DEFINED_TYPE
}

// Generates the fully-qualified name and the type key and registers the
// type in the given |scope| and also with the associated MojomDescriptor.
//
// This method is invoked when a UserDefinedType is added to its container,
// which may be either a file or a different UserDefinedType.
func (b *UserDefinedTypeBase) RegisterInScope(scope *Scope) *DuplicateNameError {
	// Register in the given scope with the given namePrefix.
	if err := scope.RegisterType(b.thisType); err != nil {
		return err
	}
	b.scope = scope

	b.fullyQualifiedName = buildDottedName(scope.fullyQualifiedName, b.simpleName)
	b.typeKey = computeTypeKey(b.fullyQualifiedName)
	scope.descriptor.typesByKey[b.typeKey] = b.thisType
	return nil
}

func (b UserDefinedTypeBase) String() string {
	attributeString := ""
	if b.attributes != nil {
		attributeString = fmt.Sprintf("%s", b.attributes.List)
	}
	return fmt.Sprintf("(%s)%s%s", b.typeKey, attributeString, b.simpleName)
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

func (b UserDefinedTypeBase) Scope() *Scope {
	return b.scope
}

func (b UserDefinedTypeBase) Identical(other UserDefinedType) bool {
	return b.typeKey == other.TypeKey()
}

// Some user-defined types, namely interfaces and structs, may act as
// namespaced scopes for declarations of constants and enums.
type DeclarationContainer struct {
	containedDeclarations *Scope
}

// Adds an enum to a this type, which must be an interface or struct.
func (c DeclarationContainer) AddEnum(mojomEnum *MojomEnum) *DuplicateNameError {
	return mojomEnum.RegisterInScope(c.containedDeclarations)
}

// Adds a declared constant to a this type, which must be an interface or struct.
func (c DeclarationContainer) AddConstant(declaredConst *UserDefinedConstant) *DuplicateNameError {
	return declaredConst.RegisterInScope(c.containedDeclarations)
}

/////////////////////////////////////////////////////////////
// Structs
/////////////////////////////////////////////////////////////
type MojomStruct struct {
	UserDefinedTypeBase
	DeclarationContainer

	fields []*StructField
}

func NewMojomStruct(simpleName string, attributes *Attributes) *MojomStruct {
	mojomStruct := new(MojomStruct)
	mojomStruct.fields = make([]*StructField, 0)
	mojomStruct.Init(simpleName, mojomStruct, attributes)
	return mojomStruct
}

func (s *MojomStruct) InitAsScope(parentScope *Scope) *Scope {
	s.containedDeclarations = NewLexicalScope(SCOPE_STRUCT, parentScope, s.simpleName, parentScope.file)
	return s.containedDeclarations
}

func (s *MojomStruct) AddField(field *StructField) {
	if field == nil {
		return
	}
	s.fields = append(s.fields, field)
}

func (MojomStruct) Kind() UserDefinedTypeKind {
	return STRUCT_TYPE
}

// This should be invoked some time after all of the fields have been added
// to the struct.
func (s *MojomStruct) ComputeFieldOrdinals() {
	// TODO: Also check that names are unique.
}

func (m MojomStruct) String() string {
	s := fmt.Sprintf("\n---------struct--------------\n")
	s += fmt.Sprintf("%s\n", m.UserDefinedTypeBase)
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
		if f.attributes != nil {
			attributesString = fmt.Sprintf("%s", f.attributes)
		}
		ordinalString := ""
		if f.declaredOrdinal >= 0 {
			ordinalString = fmt.Sprintf("@%d", f.declaredOrdinal)
		}
		str += fmt.Sprintf("%s%s %s%s", attributesString, f.fieldType,
			f.simpleName, ordinalString)
	}
	return str
}

type StructField struct {
	VariableDeclarationData

	fieldType    TypeRef
	defaultValue ValueRef
	// We save this token because it is possible that we won't be able to
	// validate the type safety of the assignment until after the resolution
	// phase and at that point we want to be able to generate a useful
	// error message.
	defaultValueFirstToken *lexer.Token
	offset                 int32
}

func NewStructField(fieldType TypeRef, name string,
	ordinal int64, attributes *Attributes, defaultValue ValueRef,
	defaultValueFirstToken *lexer.Token) *StructField {
	field := StructField{fieldType: fieldType, defaultValue: defaultValue,
		defaultValueFirstToken: defaultValueFirstToken}
	field.InitVariableDeclarationData(name, attributes, ordinal)
	return &field
}

func (f *StructField) ValidateDefaultValue(descriptor *MojomDescriptor) error {
	// TODO(rudominer) If if it is possible to validate without resolving, do
	// so now and possibly return an error. Otherwise register this field
	// with the given descritpor as something that needs to be validated
	// later after resolution.
	return nil
}

func (f StructField) String() string {
	defaultValueString := ""
	if f.defaultValue != nil {
		defaultValueString = fmt.Sprintf(" = %s", f.defaultValue)
	}
	return fmt.Sprintf("%s %s%s", f.fieldType, f.simpleName, defaultValueString)
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

	methodsByOrdinal map[uint32]*MojomMethod

	methodsByName map[string]*MojomMethod
}

func NewMojomInterface(simpleName string, attributes *Attributes) *MojomInterface {
	mojomInterface := new(MojomInterface)
	mojomInterface.methodsByOrdinal = make(map[uint32]*MojomMethod)
	mojomInterface.methodsByName = make(map[string]*MojomMethod)
	mojomInterface.Init(simpleName, mojomInterface, attributes)
	return mojomInterface
}

func (i *MojomInterface) InitAsScope(parentScope *Scope) *Scope {
	i.containedDeclarations = NewLexicalScope(SCOPE_INTERFACE, parentScope,
		i.simpleName, parentScope.file)
	return i.containedDeclarations
}

func (i *MojomInterface) AddMethod(method *MojomMethod) {
	i.methodsByName[method.simpleName] = method
}

func (MojomInterface) Kind() UserDefinedTypeKind {
	return INTERFACE_TYPE
}

var ErrOrdinalRange = errors.New("ordinal value out of range")
var ErrOrdinalDuplicate = errors.New("duplicate ordinal value")

type MethodOrdinalError struct {
	Ord            int64        // The attemted ordinal
	Method         *MojomMethod // The method with the attempted ordinal
	ExistingMethod *MojomMethod // Used if Err == ErrOrdinalDuplicate
	Err            error        // the type of error (ErrOrdinalRange, ErrOrdinalDuplicate)
}

// Make MethodOrdinalError implement error.
func (e *MethodOrdinalError) Error() string {
	switch e.Err {
	case ErrOrdinalRange:
	case ErrOrdinalDuplicate:
	}
	return ""
}

func (m *MojomInterface) ComputeMethodOrdinals() error {
	nextOrdinal := uint32(0)
	for _, method := range m.methodsByName {
		if method.declaredOrdinal < 0 {
			method.ordinal = nextOrdinal
			nextOrdinal = method.ordinal + 1
		} else {
			if method.declaredOrdinal >= math.MaxUint32 {
				return &MethodOrdinalError{Ord: method.declaredOrdinal, Method: method, Err: ErrOrdinalRange}
			}
			method.ordinal = uint32(method.declaredOrdinal)
		}
		if existingMethod, ok := m.methodsByOrdinal[method.ordinal]; ok {
			return &MethodOrdinalError{Ord: int64(method.ordinal), Method: method,
				ExistingMethod: existingMethod, Err: ErrOrdinalDuplicate}
		}
		m.methodsByOrdinal[method.ordinal] = method
	}
	return nil
}

func (m *MojomInterface) String() string {
	if m == nil {
		return "nil"
	}
	s := fmt.Sprintf("\n---------interface--------------\n")
	s += fmt.Sprintf("%s\n", m.UserDefinedTypeBase)
	s += "     Methods\n"
	s += "     -------\n"
	for _, method := range m.methodsByName {
		s += fmt.Sprintf("     %s\n", method)
	}
	return s
}

type MojomMethod struct {
	VariableDeclarationData

	// The ordinal field differs from the declaredOrdinal field
	// in VariableDeclarationData because every method eventually gets
	// assigned an ordinal whereas the declaredOrdinal is only set
	// if the user explicitly sets it in the .mojom file.
	ordinal uint32

	parameters *MojomStruct

	responseParameters *MojomStruct
}

func NewMojomMethod(name string, ordinalValue int64, params,
	responseParams *MojomStruct) *MojomMethod {
	mojomMethod := new(MojomMethod)
	mojomMethod.InitVariableDeclarationData(name, nil, ordinalValue)
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
	return fmt.Sprintf("%s(%s)%s", m.simpleName, parameterString, responseString)
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

// Adds a UnionField to this Union
func (u *MojomUnion) AddField(fieldType TypeRef, fieldName string,
	tag int64, attributes *Attributes) {
	field := UnionField{fieldType: fieldType}
	field.InitVariableDeclarationData(fieldName, attributes, tag)
	u.fields = append(u.fields, field)
}

// This should be invoked some time after all of the fields have been added
// to the union.
func (u *MojomUnion) ComputeFieldTags() {
	// TODO: Also check that names are unique.
}

func (MojomUnion) Kind() UserDefinedTypeKind {
	return UNION_TYPE
}

type UnionField struct {
	VariableDeclarationData

	fieldType TypeRef
}

/////////////////////////////////////////////////////////////
// Enums
/////////////////////////////////////////////////////////////
type MojomEnum struct {
	UserDefinedTypeBase
	DeclarationContainer

	values []*EnumValue
}

func NewMojomEnum(simpleName string, attributes *Attributes) *MojomEnum {
	mojomEnum := new(MojomEnum)
	mojomEnum.values = make([]*EnumValue, 0)
	mojomEnum.Init(simpleName, mojomEnum, attributes)
	return mojomEnum
}

// A MojoEnum is a ConcreteType
func (MojomEnum) ConcreteTypeKind() TypeKind {
	return USER_DEFINED_TYPE
}

func (*MojomEnum) AllowedAsEnumValueInitializer() bool {
	return false
}

func (MojomEnum) Kind() UserDefinedTypeKind {
	return ENUM_TYPE
}

func (e *MojomEnum) InitAsScope(parentScope *Scope) *Scope {
	e.containedDeclarations = NewLexicalScope(SCOPE_ENUM, parentScope,
		e.simpleName, parentScope.file)
	return e.containedDeclarations
}

// Adds an EnumValue to this enum
func (e *MojomEnum) AddEnumValue(name string, valueRef ValueRef,
	attributes *Attributes) *DuplicateNameError {
	enumValue := new(EnumValue)
	enumValue.Init(name, ENUM_VALUE, enumValue, valueRef, attributes)
	e.values = append(e.values, enumValue)
	enumValue.enumType = e
	return enumValue.RegisterInScope(e.containedDeclarations)
}

func (e *MojomEnum) ComputeEnumValues() {
}

func (e *MojomEnum) String() string {
	s := fmt.Sprintf("\n---------enum--------------\n")
	s += fmt.Sprintf("%s\n", e.UserDefinedTypeBase)
	s += "     Values\n"
	s += "     ------\n"
	for _, value := range e.values {
		s += fmt.Sprintf("     %s", value)
	}
	return s
}

// An EnumValue is both a ValueRef and a ConcreteValue.
type EnumValue struct {
	UserDefinedValueBase

	enumType *MojomEnum
}

// EnumValue implements ConcreteValue
func (ev *EnumValue) ValueType() ConcreteType {
	return ev.enumType
}
func (ev *EnumValue) Value() interface{} {
	return *ev
}

func (enumValue *EnumValue) AsDeclaredConstant() *UserDefinedConstant {
	return nil
}

func (enumValue *EnumValue) AsEnumValue() *EnumValue {
	return enumValue
}

func (ev *EnumValue) String() string {
	return fmt.Sprintf("%s\n", ev.UserDefinedValueBase)
}

/////////////////////////////////////////////////////////////
//Declared Values
/////////////////////////////////////////////////////////////

// User-Defined Value Kinds
type UserDefinedValueKind int

const (
	ENUM_VALUE UserDefinedValueKind = iota
	DECLARED_CONSTANT
)

func (k UserDefinedValueKind) String() string {
	switch k {
	case ENUM_VALUE:
		return "enum value"
	case DECLARED_CONSTANT:
		return "declared constant"
	default:
		panic(fmt.Sprintf("Unknown UserDefinedValueKind: %d", k))
	}
}

// A UserDefinedValue is either a UserDefinedConstant or an EnumValue
type UserDefinedValue interface {
	SimpleName() string
	FullyQualifiedName() string
	Kind() UserDefinedValueKind
	Scope() *Scope
	AsDeclaredConstant() *UserDefinedConstant
	AsEnumValue() *EnumValue
	RegisterInScope(scope *Scope) *DuplicateNameError
}

type UserDefinedValueBase struct {
	attributes         *Attributes
	thisValue          UserDefinedValue
	simpleName         string
	fullyQualifiedName string
	kind               UserDefinedValueKind
	valueKey           string
	// This is the value specified in the right-hand-side of the value
	// declaration. For an enum value it will be an integer literal. For
	// a constant declartion it may be a literal or it may be a reference.
	valueRef ValueRef
	scope    *Scope
}

// This method is invoked from the constructors for the containing types:
// NewMojomInterface, NewMojomStruct, NewMojomEnum, NewMojomUnion
func (b *UserDefinedValueBase) Init(simpleName string,
	kind UserDefinedValueKind, thisValue UserDefinedValue,
	valueRef ValueRef, attributes *Attributes) {
	b.attributes = attributes
	b.thisValue = thisValue
	b.simpleName = simpleName
	b.kind = kind
	b.valueRef = valueRef
}

func (v *UserDefinedValueBase) RegisterInScope(scope *Scope) *DuplicateNameError {
	if err := scope.RegisterValue(v.thisValue); err != nil {
		return err
	}
	v.scope = scope

	if v.thisValue.Kind() == ENUM_VALUE {
		if scope.kind != SCOPE_ENUM {
			panic("An enum value may only be registered within the scope of an enum.")
		}
		// We register an enum value twice: Once within it's enum as a scope
		// and once within its enum's scope directly. In this way the enum
		// value may be reference either with or without the name of the
		// enum as a prefix.
		if err := scope.Parent().RegisterValue(v.thisValue); err != nil {
			return err
		}
	}

	v.fullyQualifiedName = buildDottedName(scope.fullyQualifiedName, v.simpleName)
	v.valueKey = computeTypeKey(v.fullyQualifiedName)
	scope.file.Descriptor.valuesByKey[v.valueKey] = v.thisValue
	return nil
}

func (b UserDefinedValueBase) String() string {
	attributeString := ""
	if b.attributes != nil {
		attributeString = fmt.Sprintf("%s", b.attributes.List)
	}
	concreteValueString := ""
	if b.valueRef != nil {
		concreteValue := b.valueRef.ResolvedValue()
		if concreteValue != nil {
			concreteValueString = fmt.Sprintf(" = %s", concreteValue)
		}
	}
	return fmt.Sprintf("%s%s%s", attributeString, b.simpleName, concreteValueString)
}

func (b *UserDefinedValueBase) Kind() UserDefinedValueKind {
	return b.kind
}

func (b *UserDefinedValueBase) SimpleName() string {
	return b.simpleName
}

func (b *UserDefinedValueBase) FullyQualifiedName() string {
	return b.fullyQualifiedName
}

func (b UserDefinedValueBase) Scope() *Scope {
	return b.scope
}

/////////////////////////////////////////////////////////////
//Declared Constants
/////////////////////////////////////////////////////////////

// This represents a Mojom constant declaration.
type UserDefinedConstant struct {
	UserDefinedValueBase

	valueType ConcreteType
}

func NewUserDefinedConstant(name string, declaredType TypeRef, value ValueRef,
	attributes *Attributes) *UserDefinedConstant {
	constant := new(UserDefinedConstant)
	constant.Init(name, DECLARED_CONSTANT, constant, value, attributes)
	return constant
}

func (constant *UserDefinedConstant) AsDeclaredConstant() *UserDefinedConstant {
	return constant
}

func (constant *UserDefinedConstant) AsEnumValue() *EnumValue {
	return nil
}

/////////////////////////////////////////////////////////////
// Declaration Data
/////////////////////////////////////////////////////////////

type VariableDeclarationData struct {
	simpleName string
	attributes *Attributes

	// We use int64 here because valid ordinals are uint32 and we want to
	// be able to represent an unset value as -1.
	declaredOrdinal int64
}

func (v *VariableDeclarationData) InitVariableDeclarationData(simpleName string,
	attributes *Attributes, ordinal int64) {
	v.simpleName = simpleName
	v.attributes = attributes
	v.declaredOrdinal = ordinal
}

func (v *VariableDeclarationData) SimpleName() string {
	return v.simpleName
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
	Key   string
	Value ConcreteValue
}

func (ma MojomAttribute) String() string {
	return fmt.Sprintf("%s=%s ", ma.Key, ma.Value)
}
