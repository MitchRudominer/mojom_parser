package mojom

import (
	"fmt"
)

type MojomDescriptor struct {
	// Note that UserDefinedType is an interface whereas UserDefinedConstant
	// is a struct. That explains why we handle the former by value and
	// the latter by pointer.
	typesByKey       map[string]UserDefinedType
	typeKeysByFQName map[string]string

	constantsByKey       map[string]*UserDefinedConstant
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

func (d *MojomDescriptor) AddNewType(newType UserDefinedType, namespace string) {
	fullyQualifiedName := namespace + "." + newType.DeclarationData().SimpleName
	newType.SetFullyQualifiedName(fullyQualifiedName)
	d.typesByKey[newType.TypeKey()] = newType
	d.typeKeysByFQName[newType.FullyQualifiedName()] = newType.TypeKey()
}

func (d *MojomDescriptor) AddNewConstant(newConstant *UserDefinedConstant, namespace string) {
	fullyQualifiedName := namespace + "." + newConstant.declarationData.SimpleName
	newConstant.setFullyQualifiedName(fullyQualifiedName)
	d.constantsByKey[newConstant.constantKey] = newConstant
	d.constantKeysByFQName[fullyQualifiedName] = newConstant.constantKey
}

func NewMojomDescriptor() *MojomDescriptor {
	descriptor := new(MojomDescriptor)
	descriptor.typesByKey = make(map[string]UserDefinedType)
	descriptor.typeKeysByFQName = make(map[string]string)

	descriptor.constantsByKey = make(map[string]*UserDefinedConstant)
	descriptor.constantKeysByFQName = make(map[string]string)

	descriptor.MojomFiles = make([]*MojomFile, 0)

	descriptor.unresolvedTypeReferences = make([]*TypeReference, 0)
	descriptor.unresolvedConstantReferences = make([]*ConstantOccurrence, 0)
	return descriptor
}

func SprintMapValueNames(m map[string]UserDefinedType) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s\n", key, value.FullyQualifiedName())
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

	// These are lists of *top-level* types defined in the file. They do
	// not include enums and constants defined within structs, unions
	// and interfaces.
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

func (f *MojomFile) String() string {
	s := fmt.Sprintf("file name: %s\n", f.FileName)
	s += fmt.Sprintf("module: %s\n", f.ModuleNamespace)
	s += fmt.Sprintf("attributes: %s\n", f.Attributes)
	s += fmt.Sprintf("imports: %s\n", f.Imports)
	s += fmt.Sprintf("interfaces: %s\n", f.Interfaces)
	return s
}

func (f *MojomFile) AddImport(fileName string) {
	f.Imports = append(f.Imports, MojomFileReference{FileName: fileName})
}

func (f *MojomFile) AddInterface(mojomInterface *MojomInterface) {
	f.Descriptor.AddNewType(mojomInterface, f.ModuleNamespace)
	f.Interfaces = append(f.Interfaces, mojomInterface)
}

func (f *MojomFile) AddStruct(mojomStruct *MojomStruct) {
	f.Descriptor.AddNewType(mojomStruct, f.ModuleNamespace)
	f.Structs = append(f.Structs, mojomStruct)
}

func (f *MojomFile) AddEnum(mojomEnum *MojomEnum) {
	f.Descriptor.AddNewType(mojomEnum, f.ModuleNamespace)
	f.Enums = append(f.Enums, mojomEnum)
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
	Init(simpleName string, attributes *Attributes, file *MojomFile, descriptr *MojomDescriptor)
	FullyQualifiedName() string
	SetFullyQualifiedName(fqn string)
	TypeKey() string
	DeclarationData() *TypeDeclarationData
	Descriptor() *MojomDescriptor
	SupportsContainedDeclarations() bool
	Identical(other UserDefinedType) bool
}

type UserDefinedTypeBase struct {
	declarationData    *TypeDeclarationData
	fullyQualifiedName string
	typeKey            string
	file               *MojomFile
	descriptor         *MojomDescriptor
}

func (b UserDefinedTypeBase) String() string {
	s := fmt.Sprintf("fully qualified name: %s\n", b.fullyQualifiedName)
	s += fmt.Sprintf("type key: %s\n", b.typeKey)
	return s
}

func (b UserDefinedTypeBase) FullyQualifiedName() string {
	return b.fullyQualifiedName
}

// This method also computes and stores the type key
func computeTypeKey(fullyQualifiedName string) (typeKey string) {
	// TODO(rudominer) This should be the SHA1 hash of fqn instead.
	return fullyQualifiedName
}

func (b *UserDefinedTypeBase) Init(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) {
	b.declarationData = NewTypeDeclarationData(simpleName, attributes)
	b.file = file
	b.descriptor = descriptor
}

func (b *UserDefinedTypeBase) SetFullyQualifiedName(fullyQualifiedName string) {
	b.fullyQualifiedName = fullyQualifiedName
	b.typeKey = computeTypeKey(fullyQualifiedName)
}

func (b UserDefinedTypeBase) DeclarationData() *TypeDeclarationData {
	return b.declarationData
}

func (b UserDefinedTypeBase) Descriptor() *MojomDescriptor {
	return b.descriptor
}

func (b UserDefinedTypeBase) Identical(other UserDefinedType) bool {
	return b.DeclarationData() == other.DeclarationData()
}

func AddEnum(tp UserDefinedType, mojomEnum *MojomEnum) {
	if !tp.SupportsContainedDeclarations() {
		panic(fmt.Sprintf("Type %v does not support contained declarations.", tp))
	}
	tp.Descriptor().AddNewType(mojomEnum, tp.FullyQualifiedName())
	tp.DeclarationData().ContainedDeclarations.enumKeys =
		append(tp.DeclarationData().ContainedDeclarations.enumKeys, mojomEnum.typeKey)
}

func AddDeclaredConstant(tp UserDefinedType, declaredConst *UserDefinedConstant) {
	if !tp.SupportsContainedDeclarations() {
		panic(fmt.Sprintf("Type %v does not support contained declarations.", tp))
	}
	tp.Descriptor().AddNewConstant(declaredConst, tp.FullyQualifiedName())
	tp.DeclarationData().ContainedDeclarations.constantKeys =
		append(tp.DeclarationData().ContainedDeclarations.constantKeys, declaredConst.constantKey)
}

/////////////////////////////////////////////////////////////
// Structs
/////////////////////////////////////////////////////////////
type MojomStruct struct {
	UserDefinedTypeBase

	fields []StructField
}

func (s *MojomStruct) SupportsContainedDeclarations() bool {
	return true
}

func (s *MojomStruct) ComputeFieldOrdinals() {
	// TODO
}

func NewMojomStruct(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *MojomStruct {
	mojomStruct := new(MojomStruct)
	mojomStruct.fields = make([]StructField, 0)
	mojomStruct.Init(simpleName, attributes, file, descriptor)
	return mojomStruct
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

func (s *MojomStruct) AddField(field StructField) {
	s.fields = append(s.fields, field)
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

func NewMojomInterface(simpleName string,
	attributes *Attributes, file *MojomFile, descriptor *MojomDescriptor) *MojomInterface {
	mojomInterface := new(MojomInterface)
	mojomInterface.methodsByOrdinal = make(map[int]*MojomMethod)
	mojomInterface.methodsByName = make(map[string]*MojomMethod)
	mojomInterface.Init(simpleName, attributes, file, descriptor)
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
	m.methodsByName[method.SimpleName] = method
}

func (i *MojomInterface) SupportsContainedDeclarations() bool {
	return true
}

func (m *MojomInterface) ComputeMethodOrdinals() {
	// TODO
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

func (MojomInterface) Kind() UserDefinedTypeKind {
	return INTERFACE_TYPE
}

func (b *UserDefinedTypeBase) TypeKey() string {
	return b.typeKey
}

/////////////////////////////////////////////////////////////
// Unions
/////////////////////////////////////////////////////////////
type MojomUnion struct {
	UserDefinedTypeBase

	fields []UnionField
}

type UnionField struct {
	ValueDeclarationData

	fieldType Type
	tag       uint32
}

func (MojomUnion) Kind() UserDefinedTypeKind {
	return UNION_TYPE
}

func (u *MojomUnion) SupportsContainedDeclarations() bool {
	return false
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
	mojomEnum.Init(simpleName, attributes, file, descriptor)
	return mojomEnum
}

type EnumValue struct {
	ValueDeclarationData

	// The value must eventually resolve to a ConstantValue of type integer or
	// EnumConstantValue.
	value ConstantOccurrence
}

func (MojomEnum) Kind() UserDefinedTypeKind {
	return ENUM_TYPE
}

func (i *MojomEnum) SupportsContainedDeclarations() bool {
	return false
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

func (c *UserDefinedConstant) setFullyQualifiedName(fullyQualifiedName string) {
	c.fullyQualifiedName = fullyQualifiedName
	c.constantKey = computeTypeKey(fullyQualifiedName)
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
