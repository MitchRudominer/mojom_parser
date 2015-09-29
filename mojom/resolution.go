package mojom

import (
	"fmt"
)

/////////////////////////////////////////
/// Type and Value Resolution
////////////////////////////////////////

// Resolve() should be invoked after all of the parsing has been done. It
// attempts to resolve all of the entries in |d.unresolvedTypeReferences| and
// |d.unresolvedValueReferences|. Returns a non-nil error if there are any
// remaining unresolved references or if after resolution it was discovered
// that a type or value was used in an inappropriate way.
func (d *MojomDescriptor) Resolve() error {
	// Resolve the types
	unresolvedTypeReferences, err := d.resolveTypeReferences()
	if err != nil {
		// For one of the type references we discovered after resolution that
		// the resolved type was used in an inappropriate way.
		return err
	}
	numUnresolvedTypeReferences := len(unresolvedTypeReferences)

	// Resolve the values
	unresolvedValueReferences, err := d.resolveValueReferences()
	if err != nil {
		// For one of the value references we discovered after resolution that
		// the resolved value was used in an inappropriate way.
		return err
	}
	numUnresolvedValueReferences := len(unresolvedValueReferences)
	// Because values may be defined in terms of user-defined constants which
	// may themselves be defined in terms of other user-defined constants,
	// we may have to perform the value resolution step multiple times in
	// order to propogage concrete values to all declarations. To make sure that
	// this process terminates we keep iterating only as long as the number
	// of unresolved value references decreases.
	for numUnresolvedValueReferences > 0 {
		unresolvedValueReferences, _ = d.resolveValueReferences()
		if len(unresolvedValueReferences) < numUnresolvedValueReferences {
			numUnresolvedValueReferences = len(unresolvedValueReferences)
		} else {
			break
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

func (d *MojomDescriptor) resolveTypeReferences() (unresolvedReferences []*UserTypeRef, postResolutionValidationError error) {
	unresolvedReferences = make([]*UserTypeRef, len(d.unresolvedTypeReferences))
	numUnresolved := 0
	for _, ref := range d.unresolvedTypeReferences {
		if ref != nil {
			if !d.resolveTypeRef(ref) {
				unresolvedReferences[numUnresolved] = ref
				numUnresolved++
			} else {
				if postResolutionValidationError = ref.validateAfterResolution(); postResolutionValidationError != nil {
					break
				}
			}
		}
	}
	unresolvedReferences = unresolvedReferences[0:numUnresolved]
	return
}

func (d *MojomDescriptor) resolveValueReferences() (unresolvedReferences []*UserValueRef, postResolutionValidationError error) {
	unresolvedReferences = make([]*UserValueRef, len(d.unresolvedValueReferences))
	numUnresolved := 0
	for _, ref := range d.unresolvedValueReferences {
		if ref != nil {
			if !d.resolveValueRef(ref) {
				unresolvedReferences[numUnresolved] = ref
				numUnresolved++
			} else {
				if postResolutionValidationError = ref.validateAfterResolution(); postResolutionValidationError != nil {
					break
				}
			}
		}
	}
	unresolvedReferences = unresolvedReferences[0:numUnresolved]
	return
}

func (d *MojomDescriptor) resolveTypeRef(ref *UserTypeRef) (success bool) {
	ref.resolvedType = ref.scope.LookupType(ref.identifier)
	return ref.resolvedType != nil
}

// There are two steps to resolving a value. First resolve the identifier to
// to a target declaration, then resolve the target declaration to a
// concrte value.
func (d *MojomDescriptor) resolveValueRef(ref *UserValueRef) (resolved bool) {
	// Step 1: Find resolvedDeclaredValue
	if ref.resolvedDeclaredValue == nil {
		userDefinedValue := ref.scope.LookupValue(ref.identifier, ref.assigneeType)
		if userDefinedValue == nil {
			userDefinedValue = LookupBuiltInValue(ref.identifier)
		}
		if userDefinedValue == nil {
			return false
		}
		if declaredConstant := userDefinedValue.AsDeclaredConstant(); declaredConstant != nil {
			// The identifier resolves to a user-declared constant.
			ref.resolvedDeclaredValue = declaredConstant
		} else {
			// The identifier resolves to a user-declared constant.
			ref.resolvedDeclaredValue = userDefinedValue.AsEnumValue()
		}
	}

	// Step 2: Find resolvedConcreteValue.
	if declaredConstant := ref.resolvedDeclaredValue.AsDeclaredConstant(); declaredConstant != nil {
		// The identifier resolved to a user-declared constant. We use
		// the (possibly nil) resolved value of that constant as
		// resolvedConcreteValue. Since this may be nil it is possible that
		// ref is still unresolved.
		ref.resolvedConcreteValue = declaredConstant.valueRef.ResolvedConcreteValue()
	} else {
		// The identifier resolved to an enum value. We use the enum value
		// itself (not the integer value of the enum value) as the
		// resolvedConcreteValue. Since this cannot be nil we know that
		// ref is now fully resolved.s
		ref.resolvedConcreteValue = ref.resolvedDeclaredValue.AsEnumValue()
	}

	return ref.resolvedConcreteValue != nil
}

// Support for built-in values

func LookupBuiltInValue(identifier string) UserDefinedValue {
	return builtInValues[identifier]
}

type BuiltInValue int

const (
	FLOAT_INFINITY BuiltInValue = iota
	FLOAT_NEGATIVE_INFINITY
	FLOAT_NAN
	DOUBLE_INFINITY
	DOUBLE_NEGATIVE_INFINITY
	DOUBLE_NAN
)

var allBuiltInValues = []BuiltInValue{FLOAT_INFINITY, FLOAT_NEGATIVE_INFINITY,
	FLOAT_NAN, DOUBLE_INFINITY, DOUBLE_NEGATIVE_INFINITY, DOUBLE_NAN}

var builtInValues map[string]UserDefinedValue

func init() {
	builtInValues = make(map[string]UserDefinedValue, len(allBuiltInValues))
	for _, b := range allBuiltInValues {
		builtInValues[b.String()] = b
	}
}

func (b BuiltInValue) String() string {
	switch b {
	case FLOAT_INFINITY:
		return "float.INFINITY"
	case FLOAT_NEGATIVE_INFINITY:
		return "float.NEGATIVE_INFINITY"
	case FLOAT_NAN:
		return "float.NAN"
	case DOUBLE_INFINITY:
		return "double.INFINITY"
	case DOUBLE_NEGATIVE_INFINITY:
		return "double.NEGATIVE_INFINITY"
	case DOUBLE_NAN:
		return "double.NAN"
	default:
		panic(fmt.Sprintf("Unknown BuiltInValue %d", b))
	}
}

// Make BuiltInValue implement UserDefinedValue
func (BuiltInValue) AsDeclaredConstant() *UserDefinedConstant {
	return nil
}

func (BuiltInValue) AsEnumValue() *EnumValue {
	return nil
}

func (b BuiltInValue) AsBuiltinValue() *BuiltInValue {
	return &b
}

func (b BuiltInValue) SimpleName() string {
	return b.String()
}

func (b BuiltInValue) FullyQualifiedName() string {
	return b.String()
}

func (b BuiltInValue) Kind() UserDefinedValueKind {
	return BUILT_IN_VALUE
}

func (b BuiltInValue) Scope() *Scope {
	return nil
}

func (b BuiltInValue) ValueKey() string {
	return "built-in-value:" + b.String()
}

func (b BuiltInValue) RegisterInScope(scope *Scope) *DuplicateNameError {
	panic("Do not register a BuiltInValue in a scope.")
}
