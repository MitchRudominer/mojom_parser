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

func (f *MojomFile) AddInterface(mojomInterface *MojomInterface) {
	mojomInterface.RegisterInScope(f.FileScope)
	f.Interfaces = append(f.Interfaces, mojomInterface)
}

func (f *MojomFile) AddStruct(mojomStruct *MojomStruct) {
	mojomStruct.RegisterInScope(f.FileScope)
	f.Structs = append(f.Structs, mojomStruct)
}

func (f *MojomFile) AddEnum(mojomEnum *MojomEnum) {
	mojomEnum.RegisterInScope(f.FileScope)
	f.Enums = append(f.Enums, mojomEnum)
}

func (f *MojomFile) AddUnion(mojomUnion *MojomUnion) {
	mojomUnion.RegisterInScope(f.FileScope)
	f.Unions = append(f.Unions, mojomUnion)
}

func (f *MojomFile) AddConstant(declaredConst *UserDefinedConstant) {
	declaredConst.RegisterInScope(f.FileScope)
	f.Constants = append(f.Constants, declaredConst)
}

//////////////////////////////////////////////////////////////////
/// type MojomDescriptor
/// //////////////////////////////////////////////////////////////

type MojomDescriptor struct {
	// Note that UserDefinedType is an interface whereas UserDefinedConstant
	// is a struct. That explains why we handle the former by value and
	// the latter by pointer.
	typesByKey     map[string]UserDefinedType
	constantsByKey map[string]*UserDefinedConstant
	mojomFiles     []*MojomFile

	scopesByName                 map[string]*Scope
	unresolvedTypeReferences     []*TypeReference
	unresolvedConstantReferences []*ConstantOccurrence
}

func NewMojomDescriptor() *MojomDescriptor {
	descriptor := new(MojomDescriptor)

	descriptor.typesByKey = make(map[string]UserDefinedType)
	descriptor.constantsByKey = make(map[string]*UserDefinedConstant)
	descriptor.mojomFiles = make([]*MojomFile, 0)
	descriptor.scopesByName = make(map[string]*Scope)
	// The global namespace scope.
	descriptor.scopesByName[""] = NewAbstractModuleScope("", descriptor)

	descriptor.unresolvedTypeReferences = make([]*TypeReference, 0)
	descriptor.unresolvedConstantReferences = make([]*ConstantOccurrence, 0)
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
	// TODO
	return nil
}

func (d *MojomDescriptor) ContainsFile(fileName string) bool {
	return false
}

func (d *MojomDescriptor) SprintMojomFileNames() (s string) {
	for _, f := range d.mojomFiles {
		s += fmt.Sprintf("%s ", f.FileName)
	}
	return
}

func (d *MojomDescriptor) SprintUnresolvedTypeReference() (s string) {
	for _, r := range d.unresolvedTypeReferences {
		s += fmt.Sprintf("%s\n", r.LongString())
	}
	return
}

func (d *MojomDescriptor) String() string {
	return "TODO"
}

func (d *MojomDescriptor) AddMojomFile(fileName string) *MojomFile {
	mojomFile := NewMojomFile(fileName, d)
	mojomFile.Descriptor = d
	d.mojomFiles = append(d.mojomFiles, mojomFile)
	return mojomFile
}

func (d *MojomDescriptor) RegisterUnresolvedTypeReference(typeReference *TypeReference) {
	d.unresolvedTypeReferences = append(d.unresolvedTypeReferences, typeReference)
}

///////////////////////////////////////////////////////////////////////
/// Miscelleneous utilities
/// //////////////////////////////////////////////////////////////////

// This method also computes and stores the type key
func computeTypeKey(fullyQualifiedName string) (typeKey string) {
	// TODO(rudominer) This should be the SHA1 hash of fqn instead.
	return fullyQualifiedName
}

func SprintMapValueNames(m map[string]UserDefinedType) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s\n", key, value.FullyQualifiedName())
	}
	return
}
