package mojom

//////////////////////////////////////////////////
/// Generation of Field Packing and Version Data
//////////////////////////////////////////////////

// ComputeDataForGenerators() should be invoked after Resolve() has completed
// successfully. It computes the field packing and version data that will
// be used by the code generators.
func (d *MojomDescriptor) ComputeDataForGenerators() error {
	// TODO(rudominer) Implement ComputeDataForGenerators()
	return nil
}

type StructVersion struct {
	versionNumber uint32
	numFields     uint32
	numBytes      uint32
}
