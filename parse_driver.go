package main

import (
	"fmt"
	"github.com/rudominer/mojom_parser/mojom"
	"github.com/rudominer/mojom_parser/parser"
)

// FileProvider is an abstraction that allows us to mock out the file system
// in tests.
type FileProvider interface {
	ProvideContents(fileName string) (contents string)
}

type ParseResult struct {
	Err        error
	Descriptor *mojom.MojomDescriptor
}

type ParseDriver struct {
	descriptor          *mojom.MojomDescriptor
	err                 error
	fileProvider        FileProvider
	fileNamesToBeParsed []string
}

func NewParseDriver(fileProvider FileProvider) ParseDriver {
	return ParseDriver{fileProvider: fileProvider}
}

func (d *ParseDriver) ParseFiles(fileNames []string) ParseResult {
	d.descriptor = mojom.NewMojomDescriptor()
	fileNamesToBeParsed := fileNames
	for len(fileNamesToBeParsed) > 0 {
		nextFileName := fileNamesToBeParsed[0]
		d.fileNamesToBeParsed = fileNamesToBeParsed[1:]
		if !d.descriptor.ContainsFile(nextFileName) {
			contents := d.fileProvider.ProvideContents(nextFileName)
			parser := parser.NewParser(nextFileName, contents, d.descriptor)
			parser.Parse()
			if parser.Error() {
				d.err = parser.GetError()
				return ParseResult{Err: d.err, Descriptor: d.descriptor}
			}
			mojomFile := parser.GetMojomFile()
			for _, importedFile := range mojomFile.Imports {
				if !d.descriptor.ContainsFile(importedFile.FileName) {
					d.fileNamesToBeParsed = append(d.fileNamesToBeParsed, importedFile.FileName)
				}
			}

		}
	}
	if !d.descriptor.Resolve() {
		// TODO(rudominer) The error message should list the references.
		d.err = fmt.Errorf("There are still some unresolved references.")
	}
	return ParseResult{Err: d.err, Descriptor: d.descriptor}
}

func getListOfFilesFromArgs() []string {
	// TODO(rudominer)
	return nil
}

func getFileProvider() FileProvider {
	// TODO(rudominer)
	return nil
}

func writeSerializedMojomDescriptor([]byte) {
	// TODO(rudominer)
}

func main() {
	parserDriver := NewParseDriver(getFileProvider())
	result := parserDriver.ParseFiles(getListOfFilesFromArgs())
	if result.Err != nil {
		fmt.Printf("Errors: %s", result.Err.Error())
	}
	serialized := result.Descriptor.Serialize()
	writeSerializedMojomDescriptor(serialized)
}
