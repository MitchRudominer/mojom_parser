package mojom

import (
	"fmt"
)

// This file contains the types MojomFile and MojomDescriptor. These are the
// structures that are generated during parsing and then serialized and
// passed on to the backend of the Mojom Compiler.

///////////////////////////////////////////////////////////////////////
/// Type MojomFile
/// //////////////////////////////////////////////////////////////////

// A MojomFile represents the result of parsing a single .mojom file.
type MojomFile struct {
	// The associated MojomDescriptor
	Descriptor *MojomDescriptor

	// The |FileName| is (derived from) the file name of the corresponding
	// .mojom file. It is the unique identifier for this module within the
	// |mojomFilesByName| field of |Descriptor|
	FileName string

	// The module namespace is the identifier declared via the "module"
	// declaration in the .mojom file.
	ModuleNamespace string

	// Attributes declared in the Mojom file at the module level.
	Attributes *Attributes

	// The list of other MojomFiles imported by this one. The elements
	// of the array are the |FileName|s. The corresponding MojomFile may
	// be obtained from the |mojomFilesByName| field of |Descriptor|.
	Imports []string

	// The lexical scope corresponding to this file.
	FileScope *Scope

	// These are lists of *top-level* types  and constants defined in the file;
	// they do not include enums and constants defined within structs
	// and interfaces. The contained enums and constant may be found in the
	// |Enums| and |Constants|  fields of their containing object.
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
	s += fmt.Sprintf("constants: %s\n", f.Constants)
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

// A MojomDescriptor is the central object being populated by the frontend of
// the Mojom compiler. The same instance of MojomDescriptor is passed to each
// of the instances of Parser that are created by the ParseDriver while parsing
// a graph of Mojom files.  The output of ParserDriver.ParseFiles() is a
// ParseResult the main field of which is a MojomDescriptor. The MojomDescriptor
// is then serialized and passed to the backend of the Mojom compiler.
type MojomDescriptor struct {
	// All of the UserDefinedTypes keyed by type key
	typesByKey map[string]UserDefinedType

	// All of the UserDefinedValues keyed by value key
	valuesByKey map[string]UserDefinedValue

	// All of the MojomFiles in the order they were visited.
	mojomFiles []*MojomFile

	// All of the MojomFiles keyed by FileName
	mojomFilesByName map[string]*MojomFile

	// The abstract module namespace scopes keyed by scope name. These are
	// the scopes that are not lexical scopes (i.e. files, structs, interfaces)
	// but rather the ancestor's of the file scopes that are implicit in the
	// dotted name of the file's module namespace. For example if a file's
	// module namespace is "foo.bar" then the chain of ancestors of the
	// file scope is as follows:
	// [file "foo.bar"] -> [module "foo.bar"] -> [module "foo"] -> [module ""]
	// The field |abstractScopesByName| will contain the last three of these
	// scopes but not the first. The last scope [module ""] is called the
	// global scope. The reason for both a [file "foo.bar"] and a
	// [module "foo.bar"] is that multiple files might have a module namespace
	// that is a descendent of [module "foo.bar"].
	abstractScopesByName map[string]*Scope

	// When new type and value references are encountered during parsing they
	// are added to these slices. After parsing completes the resolution
	// step attempts to resolve all of these references. If these slices are
	// not empty by the time ParserDriver.ParseFiles() completes it means that
	// there are unresolved references in the .mojom files.
	unresolvedTypeReferences  []*UserTypeRef
	unresolvedValueReferences []*UserValueRef
}

func NewMojomDescriptor() *MojomDescriptor {
	descriptor := new(MojomDescriptor)

	descriptor.typesByKey = make(map[string]UserDefinedType)
	descriptor.valuesByKey = make(map[string]UserDefinedValue)
	descriptor.mojomFiles = make([]*MojomFile, 0)
	descriptor.mojomFilesByName = make(map[string]*MojomFile)
	descriptor.abstractScopesByName = make(map[string]*Scope)
	// The global namespace scope.
	descriptor.abstractScopesByName[""] = NewAbstractModuleScope("", descriptor)

	descriptor.unresolvedTypeReferences = make([]*UserTypeRef, 0)
	descriptor.unresolvedValueReferences = make([]*UserValueRef, 0)
	return descriptor
}

func (d *MojomDescriptor) getAbstractModuleScope(fullyQualifiedName string) *Scope {
	if scope, ok := d.abstractScopesByName[fullyQualifiedName]; ok {
		return scope
	}
	scope := NewAbstractModuleScope(fullyQualifiedName, d)
	d.abstractScopesByName[fullyQualifiedName] = scope
	return scope
}

func (d *MojomDescriptor) getGlobalScobe() *Scope {
	return d.abstractScopesByName[""]
}

func (d *MojomDescriptor) Serialize() []byte {
	// TODO(rudominer) Implement real serialization.
	return []byte(fmt.Sprintf("%s", d))
}

func (d *MojomDescriptor) AddMojomFile(fileName string) *MojomFile {
	mojomFile := NewMojomFile(fileName, d)
	mojomFile.Descriptor = d
	d.mojomFiles = append(d.mojomFiles, mojomFile)
	if _, ok := d.mojomFilesByName[mojomFile.FileName]; ok {
		panic(fmt.Sprintf("The file %v has already been processed.", mojomFile.FileName))
	}
	d.mojomFilesByName[mojomFile.FileName] = mojomFile
	return mojomFile
}

func (d *MojomDescriptor) RegisterUnresolvedTypeReference(typeReference *UserTypeRef) {
	d.unresolvedTypeReferences = append(d.unresolvedTypeReferences, typeReference)
}

func (d *MojomDescriptor) RegisterUnresolvedValueReference(valueReference *UserValueRef) {
	d.unresolvedValueReferences = append(d.unresolvedValueReferences, valueReference)
}

func (d *MojomDescriptor) ContainsFile(fileName string) bool {
	_, ok := d.mojomFilesByName[fileName]
	return ok
}

/////////////////////////////////////////
/// Type Resolution in a MojomDescriptor
////////////////////////////////////////

// Resolve() should be invoked after all of the parsing has been done. It
// attempts to resolve all of the entries in |d.unresolvedTypeReferences| and
// |d.unresolvedValueReferences|. Returns a non-nil error if there are any
// remaining unresolved references.
func (d *MojomDescriptor) Resolve() error {
	unresolvedTypeReferences := make([]*UserTypeRef,
		len(d.unresolvedTypeReferences))
	numUnresolvedTypeReferences := 0
	for _, ref := range d.unresolvedTypeReferences {
		if ref != nil {
			if !d.resolveTypeRef(ref) {
				unresolvedTypeReferences[numUnresolvedTypeReferences] = ref
				numUnresolvedTypeReferences++
			} else {
				if err := ref.validateAfterResolution(); ref != nil {
					// The type reference was successfully resolved but
					// after doing so we discovered that the reference
					// was used in an inappropriate way given the resolved
					// type.
					return err
				}
			}
		}
	}

	unresolvedValueReferences := make([]*UserValueRef,
		len(d.unresolvedValueReferences))
	numUnresolvedValueReferences := 0
	for _, ref := range d.unresolvedValueReferences {
		if ref != nil {
			if !d.resolveValueRef(ref) {
				unresolvedValueReferences[numUnresolvedValueReferences] = ref
				numUnresolvedValueReferences++
			} else {
				if err := ref.validateAfterResolution(); err != nil {
					// The value reference was successfully resolved but
					// after doing so we discovered that the reference
					// was used in an inappropriate way given the type
					// of the resolved value.
					return err
				}
			}
		}
	}

	d.unresolvedTypeReferences = unresolvedTypeReferences[0:numUnresolvedTypeReferences]
	d.unresolvedValueReferences = unresolvedValueReferences[0:numUnresolvedValueReferences]

	if numUnresolvedTypeReferences+numUnresolvedValueReferences == 0 {
		return nil
	}

	errorMessage := "There are still some unresolved references.\n"
	if numUnresolvedTypeReferences > 0 {
		errorMessage += "\nNo defintion found for the following types:\n"
		errorMessage += "-------------------------------------------------------\n"
		for _, ref := range d.unresolvedTypeReferences {
			errorMessage += fmt.Sprintf("%s\n", ref.LongString())
		}
	}
	if numUnresolvedValueReferences > 0 {
		errorMessage += "\nNo defintion found for the following values:\n"
		errorMessage += "-----------------------------------------------------------\n"
		for _, ref := range d.unresolvedValueReferences {
			errorMessage += fmt.Sprintf("%s\n", ref.LongString())
		}
	}
	return fmt.Errorf(errorMessage)
}

func (d *MojomDescriptor) resolveTypeRef(ref *UserTypeRef) (success bool) {
	ref.resolvedType = ref.scope.LookupType(ref.identifier)
	return ref.resolvedType != nil
}

func (d *MojomDescriptor) resolveValueRef(ref *UserValueRef) (resolved bool) {
	userDefinedValue := ref.scope.LookupValue(ref.identifier, ref.assigneeType)
	if userDefinedValue == nil {
		return false
	}
	if declaredConstant := userDefinedValue.AsDeclaredConstant(); declaredConstant != nil {
		// The identifier resolves to a user-declared constant.
		ref.resolvedDeclaredValue = declaredConstant
		// We set the resolved value of the reference to the resoled value of the
		// right-hand-side of the constant declaration.
		ref.resolvedConcreteValue = declaredConstant.valueRef.ResolvedValue()
		if ref.resolvedConcreteValue != nil {
			return true
		}
		// TODO(rudominer) We need to figure out how to handle chains of value references.
		return false
	}

	// The identifier resolves to an enum value. We se the resolved value
	// of the reference to be the enum value itself (not the integer value
	// of the enum value.)
	ref.resolvedDeclaredValue = userDefinedValue.AsEnumValue()
	ref.resolvedConcreteValue = userDefinedValue.AsEnumValue()
	return ref.resolvedConcreteValue != nil
}

//////////////////////////////////////
// Debug printing of a MojomDescriptor
//////////////////////////////////////

func (d *MojomDescriptor) debugPrintMojomFiles() (s string) {
	for _, f := range d.mojomFiles {
		s += fmt.Sprintf("\n%s\n", f)
	}
	return
}

func (d *MojomDescriptor) debugPrintUnresolvedTypeReference() (s string) {
	for _, r := range d.unresolvedTypeReferences {
		s += fmt.Sprintf("%s\n", r.LongString())
	}
	return
}

func debugPrintTypeMap(m map[string]UserDefinedType) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s %s\n", key, value.SimpleName(), value.Kind())
	}
	return
}

func debugPrintValueMap(m map[string]UserDefinedValue) (s string) {
	for key, value := range m {
		s += fmt.Sprintf("%s : %s\n", key, value.SimpleName())
	}
	return
}

func (d *MojomDescriptor) String() string {
	s := "typesByKey:\n"
	s += "----------\n"
	s += debugPrintTypeMap(d.typesByKey)
	s += "\nvaluesByKey:\n"
	s += "----------\n"
	s += debugPrintValueMap(d.valuesByKey)
	s += "\nFiles:"
	s += "\n------\n"
	s += d.debugPrintMojomFiles()
	return s
}

///////////////////////////////////////////////////////////////////////
/// Miscelleneous utilities
/// //////////////////////////////////////////////////////////////////

func computeTypeKey(fullyQualifiedName string) (typeKey string) {
	if typeKey, ok := fqnToTypeKey[fullyQualifiedName]; ok == true {
		return typeKey
	}
	typeKey = fmt.Sprintf("%d", nextKey)
	nextKey++
	fqnToTypeKey[fullyQualifiedName] = typeKey
	return
}

var fqnToTypeKey map[string]string
var nextKey int

func init() {
	fqnToTypeKey = make(map[string]string)
}
