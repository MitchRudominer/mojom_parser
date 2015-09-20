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
	ref.resolvedType = ref.scope.LookupType(ref.identifier)
	return ref.resolvedType != nil
}

func (d *MojomDescriptor) resolveConstantRef(ref *ConstantOccurrence) (resolved bool) {
	ref.resolvedConstant = ref.Scope.LookupConstant(ref.identifier)
	return ref.resolvedConstant != nil
}
