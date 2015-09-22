package mojom

import (
	"fmt"
	"strings"
)

type ScopeKind int

const (
	SCOPE_ABSTRACT_MODULE ScopeKind = iota
	SCOPE_ENUM
	SCOPE_FILE_MODULE
	SCOPE_INTERFACE
	SCOPE_STRUCT
)

func (k ScopeKind) String() string {
	switch k {
	case SCOPE_ABSTRACT_MODULE:
		return "abstract module"
	case SCOPE_ENUM:
		return "enum"
	case SCOPE_FILE_MODULE:
		return "file module"
	case SCOPE_INTERFACE:
		return "interface"
	case SCOPE_STRUCT:
		return "struct"
	default:
		panic(fmt.Sprintf("Unrecognized ScopeKind %d", k))
	}
}

type Scope struct {
	kind               ScopeKind
	shortName          string
	fullyQualifiedName string
	parentScope        *Scope
	typesByName        map[string]UserDefinedType
	valuesByName       map[string]UserDefinedValue
	// file is nil for abstract module scopes
	file       *MojomFile
	descriptor *MojomDescriptor
}

func (scope *Scope) init(kind ScopeKind, shortName string,
	fullyQualifiedName string, parentScope *Scope, descriptor *MojomDescriptor) {
	scope.kind = kind
	scope.shortName = shortName
	scope.fullyQualifiedName = fullyQualifiedName
	scope.parentScope = parentScope
	scope.typesByName = make(map[string]UserDefinedType)
	scope.valuesByName = make(map[string]UserDefinedValue)
	scope.descriptor = descriptor
}

func buildDottedName(prefix, suffix string) string {
	if len(prefix) == 0 {
		return suffix
	}
	return fmt.Sprintf("%s.%s", prefix, suffix)
}

func NewLexicalScope(kind ScopeKind, parentScope *Scope,
	shortName string, file *MojomFile) *Scope {
	scope := new(Scope)
	if file == nil {
		panic("The file must not be nil.")
	}
	scope.file = file
	var fullyQualifiedName string

	switch kind {
	case SCOPE_FILE_MODULE:
		if parentScope != nil {
			panic("A file module lexical scope cannot have a parent lexical scope.")
		}
		fullyQualifiedName = file.ModuleNamespace
		parentScope = file.Descriptor.getAbstractModuleScope(fullyQualifiedName)
	case SCOPE_INTERFACE, SCOPE_STRUCT:
		if parentScope == nil || parentScope.kind != SCOPE_FILE_MODULE {
			panic("An interface or struct lexical scope must have a parent lexical scope of type FILE_MODULE.")
		}
		fullyQualifiedName = buildDottedName(parentScope.fullyQualifiedName, shortName)
	case SCOPE_ENUM:
		if parentScope == nil || parentScope.kind == SCOPE_ABSTRACT_MODULE {
			panic("An enum lexical scope must have a parent lexical scope not an ABSTRACT_MODULE scope.")
		}
		fullyQualifiedName = buildDottedName(parentScope.fullyQualifiedName, shortName)
	case SCOPE_ABSTRACT_MODULE:
		panic("The type of a lexical scope cannot be ABSTRACT_MODULE.")
	default:
		panic(fmt.Sprintf("Unrecognized ScopeKind %d", kind))
	}

	scope.init(kind, shortName, fullyQualifiedName, parentScope, file.Descriptor)

	return scope
}

func NewAbstractModuleScope(fullyQualifiedName string, descriptor *MojomDescriptor) *Scope {
	scope := new(Scope)
	var parentScope *Scope = nil
	shortName := fullyQualifiedName
	if len(fullyQualifiedName) > 0 {
		splitName := strings.Split(fullyQualifiedName, ".")
		numSegments := len(splitName)
		shortName = splitName[numSegments-1]

		if numSegments > 1 {
			parentFullyQualifiedName := strings.Join(splitName[0:numSegments-1], ".")
			parentScope = descriptor.getAbstractModuleScope(parentFullyQualifiedName)
		} else {
			parentScope = descriptor.getGlobalScobe()
		}
	}
	scope.init(SCOPE_ABSTRACT_MODULE, shortName, fullyQualifiedName, parentScope, descriptor)
	return scope
}

func (s *Scope) Parent() *Scope {
	return s.parentScope
}

func (s *Scope) String() string {
	if s.fullyQualifiedName == "" {
		return "Global"
	}
	fileNameString := ""
	if s.file != nil {
		fileNameString = fmt.Sprintf(" in %s", s.file.FileName)
	}
	return fmt.Sprintf("%s %s%s", s.kind, s.shortName, fileNameString)
}

// This structure is used for a duplication name defintion for a type or
// a value. Exactly one of the two fields will be non-nil.
type DuplicateNameError struct {
	existingType  UserDefinedType
	existingValue UserDefinedValue
}

func (d *DuplicateNameError) ExistingType() UserDefinedType {
	return d.existingType
}

func (d *DuplicateNameError) ExistingValue() UserDefinedValue {
	return d.existingValue
}

func (scope *Scope) registerTypeWithNamePrefix(userDefinedType UserDefinedType, namePrefix string) *DuplicateNameError {
	registrationName := namePrefix + userDefinedType.SimpleName()
	if existingType := scope.typesByName[registrationName]; existingType != nil {
		return &DuplicateNameError{existingType: existingType}
	}
	scope.typesByName[registrationName] = userDefinedType
	if scope.parentScope != nil {
		if scope.kind == SCOPE_FILE_MODULE {
			if scope.parentScope.kind != SCOPE_ABSTRACT_MODULE {
				panic("The parent scope of a file module should always be an abstract module.")
			}

		} else {
			namePrefix = buildDottedName(scope.shortName, namePrefix)
		}
		if err := scope.parentScope.registerTypeWithNamePrefix(userDefinedType,
			namePrefix); err != nil {
			return err
		}
	}
	return nil
}

func (scope *Scope) registerValueWithNamePrefix(value UserDefinedValue, namePrefix string) *DuplicateNameError {
	registrationName := namePrefix + value.SimpleName()
	if existingConst := scope.valuesByName[registrationName]; existingConst != nil {
		return &DuplicateNameError{existingValue: existingConst}
	}
	scope.valuesByName[registrationName] = value
	if scope.parentScope != nil {
		if scope.kind == SCOPE_FILE_MODULE {
			if scope.parentScope.kind != SCOPE_ABSTRACT_MODULE {
				panic("The parent scope of a file module should always be an abstract module.")
			}

		} else {
			namePrefix = buildDottedName(scope.shortName, namePrefix)
		}
		if err := scope.parentScope.registerValueWithNamePrefix(
			value, namePrefix); err != nil {
			return err
		}
	}
	return nil
}

func (scope *Scope) RegisterType(userDefinedType UserDefinedType) *DuplicateNameError {
	return scope.registerTypeWithNamePrefix(userDefinedType, "")
}

func (scope *Scope) RegisterValue(value UserDefinedValue) *DuplicateNameError {
	return scope.registerValueWithNamePrefix(value, "")
}

func (scope *Scope) LookupType(name string) UserDefinedType {
	if userDefinedType, ok := scope.typesByName[name]; ok {
		return userDefinedType
	}
	if scope.parentScope == nil {
		return nil
	}
	return scope.parentScope.LookupType(name)
}
func (scope *Scope) LookupValue(name string) UserDefinedValue {
	if userDefinedValue, ok := scope.valuesByName[name]; ok {

		return userDefinedValue
	}
	if scope.parentScope == nil {
		return nil
	}
	return scope.parentScope.LookupValue(name)
}
