package mojom

import (
	"fmt"
)

///////////////////////////////////////////////////////////////////////
/// Type MojomFile
/// //////////////////////////////////////////////////////////////////

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
	Imports []string

	FileScope *Scope

	// These are lists of *top-level* types defined in the file. They do
	// not include enums and constants defined within structs, unions
	// and interfaces.
	Interfaces []*MojomInterface
	Structs    []*MojomStruct
	Unions     []*MojomUnion
	Enums      []*MojomEnum
	Constants  []*UserDefinedConstant
}

func NewMojomFile(fileName string, descriptor *MojomDescriptor) *MojomFile {
	mojomFile := new(MojomFile)
	mojomFile.FileName = fileName
	mojomFile.Descriptor = descriptor
	mojomFile.ModuleNamespace = ""
	mojomFile.Imports = make([]string, 0)
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
	s += fmt.Sprintf("structs: %s\n", f.Structs)
	s += fmt.Sprintf("enums: %s\n", f.Enums)
	return s
}

func (f *MojomFile) SetModuleNamespace(namespace string) *Scope {
	f.ModuleNamespace = namespace
	f.FileScope = NewLexicalScope(SCOPE_FILE_MODULE, nil, namespace, f)
	return f.FileScope
}

func (f *MojomFile) AddImport(fileName string) {
	f.Imports = append(f.Imports, fileName)
}

func (f *MojomFile) AddInterface(mojomInterface *MojomInterface) *DuplicateNameError {
	f.Interfaces = append(f.Interfaces, mojomInterface)
	return mojomInterface.RegisterInScope(f.FileScope)
}

func (f *MojomFile) AddStruct(mojomStruct *MojomStruct) *DuplicateNameError {
	f.Structs = append(f.Structs, mojomStruct)
	return mojomStruct.RegisterInScope(f.FileScope)
}

func (f *MojomFile) AddEnum(mojomEnum *MojomEnum) *DuplicateNameError {
	f.Enums = append(f.Enums, mojomEnum)
	return mojomEnum.RegisterInScope(f.FileScope)
}

func (f *MojomFile) AddUnion(mojomUnion *MojomUnion) *DuplicateNameError {
	f.Unions = append(f.Unions, mojomUnion)
	return mojomUnion.RegisterInScope(f.FileScope)
}

func (f *MojomFile) AddConstant(declaredConst *UserDefinedConstant) *DuplicateNameError {
	f.Constants = append(f.Constants, declaredConst)
	return declaredConst.RegisterInScope(f.FileScope)
}

//////////////////////////////////////////////////////////////////
/// type MojomDescriptor
/// //////////////////////////////////////////////////////////////

type MojomDescriptor struct {
	typesByKey  map[string]UserDefinedType
	valuesByKey map[string]UserDefinedValue
	mojomFiles  []*MojomFile

	scopesByName              map[string]*Scope
	unresolvedTypeReferences  []*UserTypeRef
	unresolvedValueReferences []*UserValueRef
}

func NewMojomDescriptor() *MojomDescriptor {
	descriptor := new(MojomDescriptor)

	descriptor.typesByKey = make(map[string]UserDefinedType)
	descriptor.valuesByKey = make(map[string]UserDefinedValue)
	descriptor.mojomFiles = make([]*MojomFile, 0)
	descriptor.scopesByName = make(map[string]*Scope)
	// The global namespace scope.
	descriptor.scopesByName[""] = NewAbstractModuleScope("", descriptor)

	descriptor.unresolvedTypeReferences = make([]*UserTypeRef, 0)
	descriptor.unresolvedValueReferences = make([]*UserValueRef, 0)
	return descriptor
}

func (d *MojomDescriptor) getAbstractModuleScope(fullyQualifiedName string) *Scope {
	if scope, ok := d.scopesByName[fullyQualifiedName]; ok {
		return scope
	}
	scope := NewAbstractModuleScope(fullyQualifiedName, d)
	d.scopesByName[fullyQualifiedName] = scope
	return scope
}

func (d *MojomDescriptor) getGlobalScobe() *Scope {
	return d.scopesByName[""]
}

func (d *MojomDescriptor) Serialize() []byte {
	return []byte(fmt.Sprintf("%s", d))
}

func (d *MojomDescriptor) ContainsFile(fileName string) bool {
	return false
}

func (d *MojomDescriptor) SprintMojomFiles() (s string) {
	for _, f := range d.mojomFiles {
		s += fmt.Sprintf("\n%s\n", f)
	}
	return
}

func (d *MojomDescriptor) SprintUnresolvedTypeReference() (s string) {
	for _, r := range d.unresolvedTypeReferences {
		s += fmt.Sprintf("%s\n", r.LongString())
	}
	return
}

func SprintTypeMap(m map[string]UserDefinedType) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s %s\n", key, value.SimpleName(), value.Kind())
	}
	return
}

func SprintValueMap(m map[string]UserDefinedValue) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s\n", key, value.SimpleName())
	}
	return
}

func (d *MojomDescriptor) String() string {
	s := "typesByKey:\n"
	s += "----------\n"
	s += SprintTypeMap(d.typesByKey)
	s += "\nvaluesByKey:\n"
	s += "----------\n"
	s += SprintValueMap(d.valuesByKey)
	s += "\nFiles:"
	s += "\n------\n"
	s += d.SprintMojomFiles()
	return s
}

func (d *MojomDescriptor) AddMojomFile(fileName string) *MojomFile {
	mojomFile := NewMojomFile(fileName, d)
	mojomFile.Descriptor = d
	d.mojomFiles = append(d.mojomFiles, mojomFile)
	return mojomFile
}

func (d *MojomDescriptor) RegisterUnresolvedTypeReference(typeReference *UserTypeRef) {
	d.unresolvedTypeReferences = append(d.unresolvedTypeReferences, typeReference)
}

func (d *MojomDescriptor) RegisterUnresolvedValueReference(valueReference *UserValueRef) {
	d.unresolvedValueReferences = append(d.unresolvedValueReferences, valueReference)
}

///////////////////////////////////////////////////////////////////////
/// Miscelleneous utilities
/// //////////////////////////////////////////////////////////////////

// This method also computes and stores the type key
func computeTypeKey(fullyQualifiedName string) (typeKey string) {
	// TODO(rudominer) This should be the SHA1 hash of fqn instead.
	return fullyQualifiedName
}
