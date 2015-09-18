package main

import (
	"fmt"
	"github.com/rudominer/mojom_parser/mojom"
	"github.com/rudominer/mojom_parser/parser"
	"io/ioutil"
	"os"
)

type FileReference struct {
	importedFrom  *mojom.MojomFile
	specifiedPath string
}

func (f FileReference) String() string {
	if f.importedFrom != nil {
		return fmt.Sprintf("file %s imported from file %s",
			f.specifiedPath, f.importedFrom.FileName)
	} else {
		return fmt.Sprintf("file %s", f.specifiedPath)
	}

}

// FileProvider is an abstraction that allows us to mock out the file system
// in tests.
type FileProvider interface {
	ProvideContents(fileReference FileReference) (contents string)
}

type OSFileProvider struct {
}

func (p OSFileProvider) ProvideContents(fileReference FileReference) (contents string) {
	data, err := ioutil.ReadFile(fileReference.specifiedPath)
	if err != nil {
		fmt.Printf("\nError while reading %s: %s\n\n",
			fileReference, err)
		os.Exit(1)
	}
	return string(data)
}

type ParseResult struct {
	Err        error
	Descriptor *mojom.MojomDescriptor
}

type ParseDriver struct {
	descriptor     *mojom.MojomDescriptor
	err            error
	fileProvider   FileProvider
	filesToProcess []FileReference
}

func NewParseDriver() ParseDriver {
	return ParseDriver{fileProvider: OSFileProvider{}}
}

func (d *ParseDriver) ParseFiles(fileNames []string) ParseResult {
	d.descriptor = mojom.NewMojomDescriptor()
	d.filesToProcess = make([]FileReference, len(fileNames))
	for i, fileName := range fileNames {
		d.filesToProcess[i] = FileReference{specifiedPath: fileName}
	}
	for len(d.filesToProcess) > 0 {
		nextFileRef := d.filesToProcess[0]
		d.filesToProcess = d.filesToProcess[1:]
		if !d.descriptor.ContainsFile(nextFileRef.specifiedPath) {
			contents := d.fileProvider.ProvideContents(nextFileRef)
			parser := parser.NewParser(nextFileRef.specifiedPath,
				contents, d.descriptor)
			parser.Parse()
			if !parser.OK() {
				d.err = fmt.Errorf("\nError while parsing %s: %s\n",
					nextFileRef, parser.Error().Error())
				return ParseResult{Err: d.err, Descriptor: d.descriptor}
			}
			mojomFile := parser.GetMojomFile()
			for _, importedFile := range mojomFile.Imports {
				if !d.descriptor.ContainsFile(importedFile) {
					d.filesToProcess = append(d.filesToProcess,
						FileReference{importedFrom: mojomFile,
							specifiedPath: importedFile})
				}
			}

		}
	}

	resolutionError := d.descriptor.Resolve()

	return ParseResult{Err: resolutionError, Descriptor: d.descriptor}
}

func writeSerializedMojomDescriptor(data []byte) {
	fmt.Println(string(data))
}

func main() {
	parserDriver := NewParseDriver()
	result := parserDriver.ParseFiles(os.Args[1:])
	if result.Err != nil {
		fmt.Printf("%s", result.Err.Error())
	}
	serialized := result.Descriptor.Serialize()
	writeSerializedMojomDescriptor(serialized)
}
