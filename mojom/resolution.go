package mojom

import (
	"fmt"
)

///////////////////////////////////////////////////////////////////////
/// Type Resolution in a MojomDescriptor
/// //////////////////////////////////////////////////////////////////

func (d *MojomDescriptor) Resolve() error {
	unresolvedTypeReferences := make([]*UserTypeRef,
		len(d.unresolvedTypeReferences))
	numUnresolvedTypeReferences := 0
	for _, ref := range d.unresolvedTypeReferences {
		if ref != nil {
			if !d.resolveTypeRef(ref) {
				unresolvedTypeReferences[numUnresolvedTypeReferences] = ref
				numUnresolvedTypeReferences++
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

func (d *MojomDescriptor) resolveTypeRef(ref *UserTypeRef) bool {
	ref.resolvedType = ref.scope.LookupType(ref.identifier)
	if ref.usedAsMapKey && ref.resolvedType != nil && ref.resolvedType.Kind() != ENUM_TYPE {
		// TODO(rudominer) Emit a resolution type error.
	}
	return ref.resolvedType != nil
}

func (d *MojomDescriptor) resolveValueRef(ref *UserValueRef) (resolved bool) {
	userDefinedValue := ref.scope.LookupValue(ref.identifier)
	if userDefinedValue == nil {
		return false
	}
	if declaredConstant := userDefinedValue.AsDeclaredConstant(); declaredConstant != nil {
		// The identifier resolves to a user-declared constant.
		ref.resolvedConstant = declaredConstant
		// We set the resolved value of the reference to the resoled value of the
		// right-hand-side of the constant declaration.
		ref.resolvedValue = declaredConstant.valueRef.ResolvedValue()
		if ref.resolvedValue != nil {
			return true
		}
		// TODO(rudominer) We need to figure out how to handle chains of value references.
		return false
	}

	// The identifier resolves to an enum value. We se the resolved value
	// of the reference to be the enum value itself (not the integer value
	// of the enum value.)
	ref.resolvedValue = userDefinedValue.AsEnumValue()
	return ref.resolvedValue != nil
}
