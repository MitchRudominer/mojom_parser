package main

import (
	"flag"
	"fmt"
	"mojom/mojom_parser/parser"
	"os"
	"strings"
)

// We construct an object to hold the result of parsing a command-line flag
// that accepts a comma-separated list of directory paths to search for mojom
// imports
type DirectoryList []string

func (dl *DirectoryList) String() string {
	return fmt.Sprintf("%v", *dl)
}

func (dl *DirectoryList) Set(args string) error {
	for _, name := range strings.Split(args, ",") {
		info, err := os.Stat(name)
		if err != nil {
			return err
		}
		if !info.IsDir() {
			return fmt.Errorf("%s is not a directory.", name)
		}
		*dl = append(*dl, name)
	}
	return nil
}

var directoryListFlag DirectoryList

func init() {
	flag.Var(&directoryListFlag, "I", "comma-separated list of directory paths to search for mojom imports")
}

func main() {
	debug := flag.Bool("debug", false, "Generate and print the parse tree.")

	flag.Parse()

	parseDriver := parser.NewDriver()
	parseDriver.SetDebugMode(*debug)
	parseDriver.SetImportDirectories(directoryListFlag)

	// Do the parsing
	result := parseDriver.ParseFiles(flag.Args())

	if result.Err != nil {
		fmt.Printf("%s", result.Err.Error())
	} else {
		fmt.Println("Parsing complete.")
	}

	// Serialize the output.
	bytes, err := result.Descriptor.Serialize()
	if err != nil {
		fmt.Println("Serialization error: %s", err)
		return
	}
	// For debug purposes at present we print out the hex representation
	// of the bytes to the console.
	// TODO(rudominer) Write the output bytes to a file.
	fmt.Println("\n\n=============================================")
	fmt.Println("\n Debug Serialized Output:")
	if bytes == nil {
		fmt.Println("bytes == nil")
	} else {
		fmt.Printf("len(bytes)=%d\n", len(bytes))
		for _, b := range bytes {
			fmt.Printf("%X ", b)
		}
	}
	fmt.Println("\n\n=============================================")
	fmt.Println("\n Pre-Serialized Go Object:")
	fmt.Printf("\n%s\n", result.Descriptor.String())
}
