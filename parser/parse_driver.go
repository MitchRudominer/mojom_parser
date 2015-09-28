package parser

import (
	"fmt"
	"github.com/rudominer/mojom_parser/mojom"
	"io/ioutil"
)

///////////////////////////////////////////////////////////////////////
/// Type ParseDriver
//////////////////////////////////////////////////////////////////////

// A ParseDriver is used to parse a list of .mojom files.
//
// Construct a new ParseDriver via MakeDriver() and then call
// ParseFiles() passing in the list of top-level .mojom files to be parsed.
// Any imported files will also be parsed.  After all files have been parsed
// the populated |MojomDescriptor| will be resovled.
//
// The returned |ParseResult| contains the populated |MojomDescriptor|.
// If there  was an error then the |Err| field will be non-nil.
//
// A ParseDriver may only be used once.
type ParseDriver struct {
	descriptor     *mojom.MojomDescriptor
	filesToProcess []FileReference

	fileProvider FileProvider
	used         bool
	debugMode    bool
}

func MakeDriver() ParseDriver {
	return ParseDriver{descriptor: mojom.NewMojomDescriptor(),
		fileProvider: OSFileProvider{}}
}

// In debug mode we print to standard out the parse tree resulting
// from each file parsing. In non-debug mode the parsers do not explicitly
// construct a parse tree.
func (d *ParseDriver) SetDebugMode(b bool) {
	d.debugMode = b
}

type ParseResult struct {
	Err        error
	Descriptor *mojom.MojomDescriptor
}

// Parses each of the given .mojom files and all of the files in the
// import graph rooted by each file. A single MojomDescriptor is created and
// populated with the result of parsing all of those files. The returned
// ParseResult contains the populuted MojomDescriptor and any error that
// occurred.
func (d *ParseDriver) ParseFiles(fileNames []string) ParseResult {
	if d.used {
		panic("An instance of ParseDriver may only be used once.")
	}
	d.used = true

	d.filesToProcess = make([]FileReference, len(fileNames))
	for i, fileName := range fileNames {
		d.filesToProcess[i] = FileReference{specifiedPath: fileName}
	}
	for len(d.filesToProcess) > 0 {
		nextFileRef := d.filesToProcess[0]
		d.filesToProcess = d.filesToProcess[1:]
		if !d.descriptor.ContainsFile(nextFileRef.specifiedPath) {
			contents, fileReadError := d.fileProvider.ProvideContents(nextFileRef)
			if fileReadError != nil {
				return ParseResult{Err: fileReadError, Descriptor: d.descriptor}
			}
			parser := MakeParser(nextFileRef.specifiedPath,
				contents, d.descriptor)
			parser.SetDebugMode(d.debugMode)
			parser.Parse()

			if d.debugMode {
				fmt.Printf("\nParseTree for %s:", nextFileRef)
				fmt.Println(parser.GetParseTree())
			}

			if !parser.OK() {
				parseError := fmt.Errorf("\nError while parsing %s: \n%s\n",
					nextFileRef, parser.GetError().Error())
				return ParseResult{Err: parseError, Descriptor: d.descriptor}
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
	ProvideContents(fileReference FileReference) (contents string, fileReadError error)
}

type OSFileProvider struct {
}

func (p OSFileProvider) ProvideContents(fileReference FileReference) (contents string, fileReadError error) {
	data, err := ioutil.ReadFile(fileReference.specifiedPath)
	if err != nil {
		fileReadError = fmt.Errorf("\nError while reading %s: %s\n\n", fileReference, err)
	} else {
		contents = string(data)
	}
	return
}
