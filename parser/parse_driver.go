package parser

import (
	"fmt"
	"io/ioutil"
	"mojo/public/tools/bindings/mojom_parser/mojom"
	"os"
	"path/filepath"
)

///////////////////////////////////////////////////////////////////////
/// Type ParseDriver
//////////////////////////////////////////////////////////////////////

// A ParseDriver is used to parse a list of .mojom files.
//
// Construct a new ParseDriver via NewDriver(), optionally SetDebugMode()
// and SetImportDirectories() and then call ParseFiles() passing in the list of
// top-level .mojom files to be parsed. Any imported files will also be parsed.
//
// We attempt to find the file named by a given path, both top-level and
// imported, using the following algorithm:
// (1) If the specified path is an absolute path use that path
// (2) Otherwise if the file was imported from another file first attempt
// to find a file with the specified path relative to the directory of the
// importing file
// (3) Otherwise if the file was imported attempt to find a file with the
// specified path relative to one of the specified import directories.
// (4) Otherwise attempt to find a file with the specified path relative to
// the current working directory.
//
// After all files have been parsed the populated |MojomDescriptor| will be
// resovled, and then Mojo serialization data will be computed for use by
// the code generators.
//
// The returned |ParseResult| contains the populated |MojomDescriptor|.
// If there  was an error then the |Err| field will be non-nil.
//
// A ParseDriver may only be used once.
type ParseDriver struct {
	fileProvider FileProvider
	importDirs   []string
	debugMode    bool
}

func NewDriver() *ParseDriver {
	fileProvider := new(OSFileProvider)
	p := ParseDriver{fileProvider: fileProvider}
	fileProvider.parseDriver = &p
	return &p
}

// In debug mode we print to standard out the parse tree resulting
// from each file parsing. In non-debug mode the parsers do not explicitly
// construct a parse tree.
func (d *ParseDriver) SetDebugMode(b bool) {
	d.debugMode = b
}

func (d *ParseDriver) SetImportDirectories(importDirectories []string) {
	d.importDirs = importDirectories
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
	if fileNames == nil {
		panic("fileNames may not be nil.")
	}
	filesToProcess := make([]*FileReference, len(fileNames))
	descriptor := mojom.NewMojomDescriptor()
	for i, fileName := range fileNames {
		filesToProcess[i] = &FileReference{specifiedPath: fileName}
	}

	for len(filesToProcess) > 0 {
		currentFile := filesToProcess[0]
		filesToProcess = filesToProcess[1:]
		if err := d.fileProvider.FindFile(currentFile); err != nil {
			return ParseResult{Err: err, Descriptor: descriptor}
		}

		if !descriptor.ContainsFile(currentFile.absolutePath) {
			contents, fileReadError := d.fileProvider.ProvideContents(currentFile)
			if fileReadError != nil {
				return ParseResult{Err: fileReadError, Descriptor: descriptor}
			}
			parser := MakeParser(currentFile.absolutePath, contents, descriptor)
			parser.SetDebugMode(d.debugMode)
			parser.Parse()

			if d.debugMode {
				fmt.Printf("\nParseTree for %s:", currentFile)
				fmt.Println(parser.GetParseTree())
			}

			if !parser.OK() {
				parseError := fmt.Errorf("\nError while parsing %s: \n%s\n",
					currentFile, parser.GetError().Error())
				return ParseResult{Err: parseError, Descriptor: descriptor}
			}
			mojomFile := parser.GetMojomFile()
			for _, importedFile := range mojomFile.Imports {
				filesToProcess = append(filesToProcess,
					&FileReference{importedFrom: currentFile, specifiedPath: importedFile})
			}
		}
	}

	// Perform type and value resolution
	if resolutionError := descriptor.Resolve(); resolutionError != nil {
		return ParseResult{Err: resolutionError, Descriptor: descriptor}
	}

	// Compute data for generators.
	computationError := descriptor.ComputeDataForGenerators()
	return ParseResult{Err: computationError, Descriptor: descriptor}
}

type FileReference struct {
	importedFrom  *FileReference
	specifiedPath string
	absolutePath  string
	directoryPath string
}

func (f FileReference) String() string {
	if f.importedFrom != nil {
		return fmt.Sprintf("file %s imported from file %s.",
			f.specifiedPath, f.importedFrom.specifiedPath)
	} else {
		return fmt.Sprintf("file %s", f.absolutePath)
	}
}

// FileProvider is an abstraction that allows us to mock out the file system
// in tests.
type FileProvider interface {
	ProvideContents(fileRef *FileReference) (contents string, fileReadError error)
	FindFile(fileRef *FileReference) error
}

type OSFileProvider struct {
	parseDriver *ParseDriver
}

func (p OSFileProvider) ProvideContents(fileRef *FileReference) (contents string, fileReadError error) {
	data, err := ioutil.ReadFile(fileRef.absolutePath)
	if err != nil {
		fileReadError = fmt.Errorf("\nError while reading %s: %s\n\n", fileRef, err)
	} else {
		contents = string(data)
	}
	return
}

func (p *OSFileProvider) FindFile(fileRef *FileReference) (err error) {
	// If this FileReference has already been processed there is nothing to do.
	if len(fileRef.absolutePath) > 0 {
		return
	}

	// If the specified path is already absolute we use that path.
	if filepath.IsAbs(fileRef.specifiedPath) {
		fileRef.absolutePath = fileRef.specifiedPath
		fileRef.directoryPath = filepath.Dir(fileRef.absolutePath)
		return
	}

	// If the file was imported from another file...
	if fileRef.importedFrom != nil {
		// First attempt to find the file relative to the directory of the
		// importing file.
		attemptedName := filepath.Join(fileRef.importedFrom.directoryPath, fileRef.specifiedPath)
		if isFile(attemptedName) {
			fileRef.absolutePath = attemptedName
			fileRef.directoryPath = filepath.Dir(fileRef.absolutePath)
			return
		}

		// then search in the specified import directories.
		if p.parseDriver.importDirs != nil {
			for _, dir := range p.parseDriver.importDirs {
				attemptedName := filepath.Join(dir, fileRef.specifiedPath)
				if isFile(attemptedName) {
					fileRef.absolutePath = attemptedName
					fileRef.directoryPath = filepath.Dir(fileRef.absolutePath)
					return
				}
			}

		}
	}

	// Finally look in the current working directory.
	if isFile(fileRef.specifiedPath) {
		if fileRef.absolutePath, err = filepath.Abs(fileRef.specifiedPath); err != nil {
			return err
		}
		fileRef.directoryPath = filepath.Dir(fileRef.absolutePath)
		return
	}

	return fmt.Errorf("File not found: %s", fileRef)
}

func isFile(path string) bool {
	info, err := os.Stat(path)
	if err != nil {
		return false
	}
	return !info.IsDir()
}
