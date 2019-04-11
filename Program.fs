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
open System.IO

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
    oops: bool;
}

let semVer = "v0.9.4"  // Following-on from Go version

let rec parseCommandLineInner args optionsSoFar = 
    match args with 
    | [] -> optionsSoFar
    | "--dumpfile"::xs ->
        match xs with
        | [] ->
            printfn "ERROR: No dumpfile specified"
            let newOptionsSoFar = { optionsSoFar with oops=true }
            parseCommandLineInner [] newOptionsSoFar
        | x::xss ->
            let newOptionsSoFar = { optionsSoFar with dumpfilename=x}
            parseCommandLineInner xss newOptionsSoFar
    | "--extract"::xs ->
        let newOptionsSoFar = { optionsSoFar with extract=ExtractIt }
        parseCommandLineInner xs newOptionsSoFar
    | "--help"::xs ->
        printfn "UsageL LoadFS --help|--dumpfile <filename> [--version] [--extract] [--ignoreErrors] [--list] [--summary]"
        let newOptionsSoFar = { optionsSoFar with oops=true }
        parseCommandLineInner [] newOptionsSoFar
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
        oops = false;
        }
    parseCommandLineInner args defaultOptions

type dgByte = byte
type dgWord = uint16
type dgDword = uint32
type recordTypeT = 
    | START = 0
    | FSB = 1
    | NB = 2
    | UDA = 3
    | ACL = 4
    | LINK = 5
    | START_BLOCK = 6
    | DATA_BLOCK = 7
    | END_BLOCK = 8
    | END = 9
type recordHeader = {
    recordType: recordTypeT;
    recordLength: int;
    }
type SOD = {
    header: recordHeader;
    dumpFormatRevision: dgWord;
    dumpTimeSecs: dgWord;
    dumpTimeMins: dgWord;
    dumpTimeHours: dgWord;
    dumpTimeDay: dgWord;
    dumpTimeMonth: dgWord;
    dumpTimeYear: dgWord;
    }
type FSTATentryType = 
    | FLNK = 0
    | FDIR = 12
    | FDMP = 64 // inferred
    | FSTF = 67
    | FTXT = 68
    | FPRV = 74
    | FPRG = 87
type dataHeader = {
    header: recordHeader;
    byteAddress: dgWord;
    byteLength: dgWord;
    alignmentCount: dgWord;
    }



[<EntryPoint>]
let main argv =
    let options = parseCommandLine (argv |> Array.toList)
    match options.oops with
    | true -> 
        Environment.Exit 1
        1
    | false ->
        match options.dumpfilename with 
        | "" ->
            printfn "ERROR: No DUMP file specified"
            Environment.Exit 1
            1
        | _ -> 
            printfn "DEBUG: Processing DUMP file %s" options.dumpfilename
            let dfStream = File.OpenRead options.dumpfilename // TODO error handling
            let bufferSize = 512
            let mutable buffer : byte[] = Array.zeroCreate bufferSize

            0 // return an integer exit code

