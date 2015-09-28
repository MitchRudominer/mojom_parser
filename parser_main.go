package main

import (
	"flag"
	"fmt"
	"github.com/rudominer/mojom_parser/parser"
)

func main() {
	debug := flag.Bool("debug", false, "Generate and print the parse tree.")
	flag.Parse()

	parseDriver := parser.MakeDriver()
	parseDriver.SetDebugMode(*debug)
	result := parseDriver.ParseFiles(flag.Args())
	if result.Err != nil {
		fmt.Printf("%s", result.Err.Error())
	}
	// For now all the parser does with the generated MojomDescriptor is
	// to print its String() to standard out.
	// TODO(rudominer) Instead we will want to print the binary serialized
	// form to a file to be consumed by the next stage of the pipeline.
	fmt.Printf("\n%s\n", string(result.Descriptor.Serialize()))
}
