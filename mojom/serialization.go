package mojom

import (
	"mojo/public/go/bindings"
	"mojom/mojom_parser/generated/mojom_files"
)

//////////////////////////////////////////////////
/// Mojom Descriptor Serialization
//////////////////////////////////////////////////

// Serializes the MojomDescriptor into a binary form that is passed to the
// backend of the compiler in order to invoke the code generators.
// To do this we use Mojo serialization.
func (d *MojomDescriptor) Serialize() (bytes []byte, err error) {
	fileGraph := translateDescriptor(d)
	encoder := bindings.NewEncoder()
	fileGraph.Encode(encoder)
	bytes, _, err = encoder.Data()
	return
}

func translateDescriptor(d *MojomDescriptor) *mojom_files.MojomFileGraph {
	fileGraph := mojom_files.MojomFileGraph{}
	fileGraph.Files = make(map[string]mojom_files.MojomFile)
	fileGraph.Files["test"] = mojom_files.MojomFile{}
	return &fileGraph
}
