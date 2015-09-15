package mojom

import (
	"fmt"
)

type MojomDescriptor struct {
	typesByKey       map[string]UserDefinedType
	typeKeysByFQName map[string]string

	constantsByKey       map[string]UserDefinedType
	constantKeysByFQName map[string]string

	MojomFiles []*MojomFile

	unresolvedTypeReferences     []*TypeReference
	unresolvedConstantReferences []*ConstantOccurrence
}

func (d *MojomDescriptor) Serialize() []byte {
	return nil
}

func (d *MojomDescriptor) ContainsFile(fileName string) bool {
	return false
}

func (d *MojomDescriptor) AddNewType(newType UserDefinedType) {
	d.typesByKey[newType.GetTypeKey()] = newType
	d.typeKeysByFQName[newType.GetFullyQualifiedName()] = newType.GetTypeKey()
}

func NewMojomDescriptor() *MojomDescriptor {
	descriptor := new(MojomDescriptor)
	descriptor.typesByKey = make(map[string]UserDefinedType)
	descriptor.typeKeysByFQName = make(map[string]string)

	descriptor.constantsByKey = make(map[string]UserDefinedType)
	descriptor.constantKeysByFQName = make(map[string]string)

	descriptor.MojomFiles = make([]*MojomFile, 0)

	descriptor.unresolvedTypeReferences = make([]*TypeReference, 0)
	descriptor.unresolvedConstantReferences = make([]*ConstantOccurrence, 0)
	return descriptor
}

func SprintMapValueNames(m map[string]UserDefinedType) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s\n", key, value.GetFullyQualifiedName())
	}
	return
}

func (d *MojomDescriptor) SprintMojomFileNames() (s string) {
	for _, f := range d.MojomFiles {
		if f == nil {
			s += "nil "
		} else {
			s += f.FileName
		}
	}
	return
}

func (d *MojomDescriptor) String() string {
	s := fmt.Sprintf("typesByKey:\n %s", SprintMapValueNames(d.typesByKey))
	s += fmt.Sprintf("typeKeysByFQName:\n %v", d.typeKeysByFQName)
	s += fmt.Sprintf("\nMojomFiles:\n %s\n", d.SprintMojomFileNames())
	return s
}

func (d *MojomDescriptor) AddMojomFile(fileName string) *MojomFile {
	mojomFile := NewMojomFile(fileName)
	mojomFile.Descriptor = d
	d.MojomFiles = append(d.MojomFiles, mojomFile)
	return mojomFile
}

func (d *MojomDescriptor) Resolve() (resolved bool) {
	typesResolved := true
	for i, ref := range d.unresolvedTypeReferences {
		if ref != nil {
			if d.resolveTypeRef(ref) {
				d.unresolvedTypeReferences[i] = nil
			} else {
				typesResolved = false
			}
		}
	}
	if typesResolved {
		d.unresolvedTypeReferences = d.unresolvedTypeReferences[0:0]
	}

	constantsResolved := true
	for i, ref := range d.unresolvedConstantReferences {
		if ref != nil {
			if d.resolveConstantRef(ref) {
				d.unresolvedConstantReferences[i] = nil
			} else {
				constantsResolved = false
			}
		}
	}
	if constantsResolved {
		d.unresolvedTypeReferences = d.unresolvedTypeReferences[0:0]
	}
	return typesResolved && constantsResolved
}

func (d *MojomDescriptor) resolveTypeRef(ref *TypeReference) bool {
	scope := ref.Scope
	for scope != nil {
		if key, ok := d.typeKeysByFQName[scope.FullyQualifiedName+ref.identifier]; ok {
			ref.resolvedType = d.typesByKey[key]
			return true
		}
		scope = scope.ParentScope
	}
	return false
}

func (d *MojomDescriptor) resolveConstantRef(ref *ConstantOccurrence) (resolved bool) {
	return false
}

func (d *MojomDescriptor) resolveTypeRefInScope(ref *TypeReference, scope *Scope) bool {
	if key, ok := d.typeKeysByFQName[scope.FullyQualifiedName+ref.identifier]; ok {
		ref.resolvedType = d.typesByKey[key]
		return true
	}
	return false
}

type MojomFile struct {
	Descriptor *MojomDescriptor

	// The |FileName| is (derived from) the file name of the corresponding
	// .mojom file. It is the unique identifier for this module within the
	// MojomFileGraph
	FileName string

	// The module namespace is the identifier declared via the "module"
	// declaration in the .mojom file.
	ModuleNamespace string

	// Attributes declared in the Mojom file at the module level.
	Attributes *Attributes

	// The list of other MojomFiles imported by this one. The elements
	// of the array are the |module_name|s and the associated module may
	// be retrieved from the  MojomFileGraph.
	Imports []MojomFileReference

	Interfaces []*MojomInterface
	Structs    []*MojomStruct
	Unions     []*MojomUnion
	Enums      []*MojomEnum
	Constants  []*UserDefinedConstant
}

func NewMojomFile(fileName string) *MojomFile {
	mojomFile := new(MojomFile)
	mojomFile.FileName = fileName
	mojomFile.ModuleNamespace = ""
	mojomFile.Imports = make([]MojomFileReference, 0)
	mojomFile.Interfaces = make([]*MojomInterface, 0)
	mojomFile.Structs = make([]*MojomStruct, 0)
	mojomFile.Unions = make([]*MojomUnion, 0)
	mojomFile.Enums = make([]*MojomEnum, 0)
	mojomFile.Constants = make([]*UserDefinedConstant, 0)
	return mojomFile
}

func (m *MojomFile) String() string {
	s := fmt.Sprintf("file name: %s\n", m.FileName)
	s += fmt.Sprintf("module: %s\n", m.ModuleNamespace)
	s += fmt.Sprintf("attributes: %s\n", m.Attributes)
	s += fmt.Sprintf("imports: %s\n", m.Imports)
	s += fmt.Sprintf("interfaces: %s\n", m.Interfaces)
	return s
}

func (m *MojomFile) AddImport(fileName string) {
	m.Imports = append(m.Imports, MojomFileReference{FileName: fileName})
}

func (m *MojomFile) AddNewType(newType UserDefinedType, simpleName string,
	attributes *Attributes) {
	fullyQualifiedName := m.ModuleNamespace + "." + simpleName
	declarationData := NewDeclarationData(simpleName, attributes)
	newType.Init(fullyQualifiedName, declarationData)
	if m.Descriptor != nil {
		m.Descriptor.AddNewType(newType)
	}
}

func (m *MojomFile) AddInterface(simpleName string, attributes *Attributes) *MojomInterface {
	mojomInterface := NewMojomInterface()
	m.AddNewType(mojomInterface, simpleName, attributes)
	m.Interfaces = append(m.Interfaces, mojomInterface)
	return mojomInterface
}

type MojomFileReference struct {
	FileName string
	File     *MojomFile
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
	Init(fullyQualifiedName string, d *DeclarationData)
	GetFullyQualifiedName() string
	GetTypeKey() string
	DeclarationData() *DeclarationData
	Identical(other UserDefinedType) bool
}

type UserDefinedTypeBase struct {
	declarationData    *DeclarationData
	fullyQualifiedName string
	typeKey            string
}

func (b UserDefinedTypeBase) String() string {
	s := fmt.Sprintf("fully qualified name: %s\n", b.fullyQualifiedName)
	s += fmt.Sprintf("type key: %s\n", b.typeKey)
	return s
}

func (b UserDefinedTypeBase) GetFullyQualifiedName() string {
	return b.fullyQualifiedName
}

// This method also computes and stores the type key
func computeTypeKey(fullyQualifiedName string) (typeKey string) {
	// TODO(rudominer) This should be the SHA1 hash of fqn instead.
	return fullyQualifiedName
}

func (b *UserDefinedTypeBase) Init(fullyQualifiedName string, d *DeclarationData) {
	b.fullyQualifiedName = fullyQualifiedName
	b.typeKey = computeTypeKey(fullyQualifiedName)
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

func NewMojomStruct() *MojomStruct {
	mojomStruct := new(MojomStruct)
	mojomStruct.fields = make([]StructField, 0)
	return mojomStruct
}

func (s *MojomStruct) AddField(fieldType Type, name string,
	ordinalValue int, attributes *Attributes) {
	field := StructField{fieldType: fieldType}
	field.SimpleName = name
	field.DeclaredOrdinal = ordinalValue
	field.Attributes = attributes
	s.fields = append(s.fields, field)
}

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

func NewMojomInterface() *MojomInterface {
	mojomInterface := new(MojomInterface)
	mojomInterface.methodsByOrdinal = make(map[int]*MojomMethod)
	mojomInterface.methodsByName = make(map[string]*MojomMethod)
	return mojomInterface
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

func (m *MojomInterface) AddMethod(method *MojomMethod) {
	m.methodsByName[method.declarationData.SimpleName] = method
}

func (m *MojomInterface) ComputeMethodOrdinals() {
	// TODO
}

type MojomMethod struct {
	declarationData *DeclarationData

	parameters *MojomStruct

	responseParameters *MojomStruct
}

func NewMojomMethod(name string, ordinalValue int, params,
	responseParams *MojomStruct) *MojomMethod {
	mojomMethod := new(MojomMethod)
	mojomMethod.declarationData = NewDeclarationData(name, nil)
	if ordinalValue >= 0 {
		mojomMethod.declarationData.DeclaredOrdinal = ordinalValue
	}
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
	return fmt.Sprintf("%s(%s)%s", m.declarationData.SimpleName, parameterString, responseString)
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
type UserDefinedConstant struct {
	DeclarationData

	// The type must be a string, bool, float, double, or integer type.
	valueType Type

	// The value must eventually resolve to the same type as |type|.
	value ConstantOccurrence
}

/////////////////////////////////////////////////////////////
// Declaration Data
/////////////////////////////////////////////////////////////
type DeclarationData struct {
	SimpleName            string
	Attributes            *Attributes
	DeclaredOrdinal       int
	ContainedDeclarations *ContainedDeclarations
}

func NewDeclarationData(simpleName string, attributes *Attributes) *DeclarationData {
	declarationData := new(DeclarationData)
	declarationData.SimpleName = simpleName
	declarationData.Attributes = attributes
	return declarationData
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
