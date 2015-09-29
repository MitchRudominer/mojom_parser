package mojom

import (
	"fmt"
)

//////////////////////////////////////////////////
/// Mojom Descriptor Serialization
//////////////////////////////////////////////////

// Serializes the MojomDescriptor into a binary form that is passed to the
// backend of the compiler in order to invoke the code generators.
func (d *MojomDescriptor) Serialize() []byte {
	// TODO(rudominer) Implement MojomDescriptor serialization.
	return []byte(fmt.Sprintf("%s", d))
}
