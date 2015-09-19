package mojom

import (
	"fmt"
)

///////////////////////////////////////////////////////////////////////
/// Type Resolution in a MojomDescriptor
/// //////////////////////////////////////////////////////////////////

func (d *MojomDescriptor) Resolve() error {
	unresolvedTypeReferences := make([]*TypeReference,
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

	unresolvedConstantReferences := make([]*ConstantOccurrence,
		len(d.unresolvedConstantReferences))
	numUnresolvedConstantReferences := 0
	for _, ref := range d.unresolvedConstantReferences {
		if ref != nil {
			if !d.resolveConstantRef(ref) {
				unresolvedConstantReferences[numUnresolvedConstantReferences] = ref
				numUnresolvedConstantReferences++
			}
		}
	}

	d.unresolvedTypeReferences = unresolvedTypeReferences[0:numUnresolvedTypeReferences]
	d.unresolvedConstantReferences = unresolvedConstantReferences[0:numUnresolvedConstantReferences]

	if numUnresolvedTypeReferences+numUnresolvedConstantReferences == 0 {
		return nil
	}

	errorMessage := "There are still some unresolved references.\n"
	if numUnresolvedTypeReferences > 0 {
		errorMessage += "\nNo type defintions found for the following identifiers:\n"
		errorMessage += "-------------------------------------------------------\n"
		for _, ref := range d.unresolvedTypeReferences {
			errorMessage += fmt.Sprintf("%s\n", ref.LongString())
		}
	}
	if numUnresolvedConstantReferences > 0 {
		errorMessage += "\nNo constant defintions found for the following identifiers:\n"
		errorMessage += "-----------------------------------------------------------\n"
		for _, ref := range d.unresolvedConstantReferences {
			errorMessage += fmt.Sprintf("%s\n", ref.LongString())
		}
	}
	return fmt.Errorf(errorMessage)
}

func (d *MojomDescriptor) resolveTypeRef(ref *TypeReference) bool {
	scope := ref.scope
	for scope != nil {
		if resolved := scope.typesByName[ref.identifier]; resolved != nil {
			ref.resolvedType = resolved
			return true
		}
		scope = scope.Parent()
	}
	// Try one more time in the global namespace.
	if key, ok := d.typeKeysByFQName[ref.identifier]; ok {
		ref.resolvedType = d.typesByKey[key]
		return true
	}
	return false
}

func (d *MojomDescriptor) resolveConstantRef(ref *ConstantOccurrence) (resolved bool) {
	return false
}
