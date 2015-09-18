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

	errorMessage := "There are still some unresolved references. " +
		"Perhaps you are missing an import statment?\n"
	if numUnresolvedTypeReferences > 0 {
		errorMessage += "The following type references are unresolved:\n"
		for _, ref := range d.unresolvedTypeReferences {
			errorMessage += fmt.Sprintf("%s\n", ref.FullString())
		}
	}
	if numUnresolvedConstantReferences > 0 {
		errorMessage += "\nThe following constant references are unresolved:\n"
		for _, ref := range d.unresolvedConstantReferences {
			errorMessage += fmt.Sprintf("%s\n", ref.FullString())
		}
	}
	return fmt.Errorf(errorMessage)
}

func (d *MojomDescriptor) resolveTypeRef(ref *TypeReference) bool {
	scope := ref.scope
	fmt.Println("***** Resolving " + ref.identifier)
	for scope != nil {
		fmt.Println("********Trying  " + scope.String())
		if resolved := scope.typesByName[ref.identifier]; resolved != nil {
			ref.resolvedType = resolved
			fmt.Println("worked!")
			return true
		}
		scope = scope.Parent()
	}
	// Try one more time in the global namespace.
	fmt.Println("********Trying Global")
	if key, ok := d.typeKeysByFQName[ref.identifier]; ok {
		ref.resolvedType = d.typesByKey[key]
		fmt.Println("worked!")
		return true
	}
	return false
}

func (d *MojomDescriptor) resolveConstantRef(ref *ConstantOccurrence) (resolved bool) {
	return false
}
