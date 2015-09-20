package mojom

import (
	"fmt"
	"strings"
)

type ScopeKind int

const (
	SCOPE_ABSTRACT_MODULE ScopeKind = iota
	SCOPE_FILE_MODULE
	SCOPE_INTERFACE
	SCOPE_STRUCT
)

func (k ScopeKind) String() string {
	switch k {
	case SCOPE_ABSTRACT_MODULE:
		return "abstract module"
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
	constantsByName    map[string]*UserDefinedConstant
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
	scope.constantsByName = make(map[string]*UserDefinedConstant)
	scope.descriptor = descriptor
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
		fullyQualifiedName = parentScope.fullyQualifiedName + "." + shortName
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
	return fmt.Sprintf("%s %s", s.kind, s.shortName)
}

/*

func (s *Scope) Kind() ScopeKind {
	return s.kind
}

func (s *bstractScope) ShortName() string {
	return s.shortName
}

func (s *bstractScope) FullyQualifiedName() string {
	return s.fullyQualifiedName
}

func (s *Scope) File() *MojomFile {
	return s.file
}
*/

func (scope *Scope) registerTypeWithNamePrefix(userDefinedType UserDefinedType, namePrefix string) {
	fmt.Printf("Registering type with name %s in scope %s.\n", namePrefix+userDefinedType.SimpleName(), scope)
	scope.typesByName[namePrefix+userDefinedType.SimpleName()] = userDefinedType
	if scope.parentScope != nil {
		if scope.kind == SCOPE_FILE_MODULE {
			if scope.parentScope.kind != SCOPE_ABSTRACT_MODULE {
				panic("The parent scope of a file module should always be an abstract module.")
			}

		} else {
			namePrefix = scope.shortName + "." + namePrefix
		}
		scope.parentScope.registerTypeWithNamePrefix(userDefinedType, namePrefix)
	}
}

func (scope *Scope) registerConstantWithNamePrefix(userDefinedConstant *UserDefinedConstant, namePrefix string) {
	scope.constantsByName[namePrefix+userDefinedConstant.simpleName] = userDefinedConstant
	if scope.parentScope != nil {
		if scope.kind == SCOPE_FILE_MODULE {
			if scope.parentScope.kind != SCOPE_ABSTRACT_MODULE {
				panic("The parent scope of a file module should always be an abstract module.")
			}

		} else {
			namePrefix = scope.shortName + "." + namePrefix
		}
		scope.parentScope.registerConstantWithNamePrefix(userDefinedConstant, namePrefix)
	}
}

func (scope *Scope) RegisterType(userDefinedType UserDefinedType) {
	scope.registerTypeWithNamePrefix(userDefinedType, "")
}

func (scope *Scope) RegisterConstant(userDefinedConstant *UserDefinedConstant) {
	scope.registerConstantWithNamePrefix(userDefinedConstant, "")
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
func (scope *Scope) LookupConstant(name string) *UserDefinedConstant {
	if userDefinedConstant, ok := scope.constantsByName[name]; ok {

		return userDefinedConstant
	}
	if scope.parentScope == nil {
		return nil
	}
	return scope.parentScope.LookupConstant(name)
}
