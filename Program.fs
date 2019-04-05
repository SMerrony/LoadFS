(*
MIT License

Copyright (c) 2019 Stephen Merrony

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open System

type ExtractOption = ExtractIt | DontExtract
type ErrorsOption = IgnoreErrors | StopOnError
type ListOption = ListFiles | DontListFiles
type SummaryOption = WithSummary | WithoutSummary
type VerboseOption = VerboseOutput | TerseOutput
type CommandLineOptions = {
    dumpfilename: string;
    errors: ErrorsOption;
    extract: ExtractOption;
    list: ListOption;
    summary: SummaryOption;
    verbose: VerboseOption;
}

let semVer = "v0.9.4"

let rec parseCommandLineInner args optionsSoFar = 
    match args with 
    | [] -> optionsSoFar
    | "--dumpfile"::xs ->
        match xs with
        | [] ->
            printfn "ERROR: No dumpfile specified"
            parseCommandLineInner xs optionsSoFar
        | x::xss ->
            let newOptionsSoFar = { optionsSoFar with dumpfilename=x}
            parseCommandLineInner xss newOptionsSoFar
    | "--extract"::xs ->
        let newOptionsSoFar = { optionsSoFar with extract=ExtractIt }
        parseCommandLineInner xs newOptionsSoFar
    | "--help"::xs ->
        printfn "UsageL LoadFS --help|--dumpfile=<filename> [--version] [--extract] [--ignoreErrors] [--list] [--summary]"
        parseCommandLineInner xs optionsSoFar
    | "--ignoreErrors"::xs ->
        let newOptionsSoFar = { optionsSoFar with errors=IgnoreErrors }
        parseCommandLineInner xs newOptionsSoFar
    | "--list"::xs ->
        let newOptionsSoFar = { optionsSoFar with list=ListFiles }
        parseCommandLineInner xs newOptionsSoFar
    | "--summary"::xs ->
        let newOptionsSoFar = { optionsSoFar with summary=WithSummary }
        parseCommandLineInner xs newOptionsSoFar
    | "--verbose"::xs ->
        let newOptionsSoFar = { optionsSoFar with verbose=VerboseOutput }
        parseCommandLineInner xs newOptionsSoFar
    | "--version"::xs ->
        printfn "LoadFS version: %s" semVer
        parseCommandLineInner xs optionsSoFar
    | x::xs -> // handle unrecognized option
        printfn "ERROR: Invalid option '%s'" x
        parseCommandLineInner xs optionsSoFar

let parseCommandLine args = 
    let defaultOptions = {
        dumpfilename = "";
        errors = StopOnError;
        extract = DontExtract;
        list = DontListFiles;
        summary = WithoutSummary;
        verbose = TerseOutput;
        }
    parseCommandLineInner args defaultOptions

[<EntryPoint>]
let main argv =
    let options = parseCommandLine (argv |> Array.toList)
    match options.dumpfilename with 
    | "" -> 
        printfn "ERROR: No DUMP file specified"
        1
    | _ -> 
        printfn "DEBUG: Processing DUMP file %s" options.dumpfilename

        0 // return an integer exit code
