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

let readBlob (numBytes : int) (reader : BinaryReader) (desc : string) =
    let mutable buffer : byte[] = Array.zeroCreate numBytes
    let bytesRead = reader.Read( buffer, 0, numBytes ) // TODO handle error
    buffer

let readHeader (reader : BinaryReader) : recordHeader =
    let twoBytes = readBlob 2 reader "header"
    let recordTypeAsInt = int ((twoBytes.[0] >>> 2) &&& 0xffuy)
    {
        recordType = enum recordTypeAsInt;
        recordLength = int (((twoBytes.[0] &&& 0x03uy) <<< 8 ) ||| twoBytes.[1]);
    }

let readWord (reader : BinaryReader) : dgWord = 
    let twoBytes = readBlob 2 reader "DG Word"
    uint16 twoBytes.[0] <<< 8 ||| uint16 twoBytes.[1]

let readSOD dumpStream : SOD =
    let hdr = readHeader dumpStream
    match hdr.recordType with 
    | recordTypeT.START ->
        printfn "DEBUG: Found header for SOD"  
    | _ ->
        printfn "ERROR: This does not appear to be an AOS/VS DUMP_II or DUMP_III file (No SOD record found)."
        Environment.Exit 1
    {
        header = hdr;
        dumpFormatRevision = readWord dumpStream;
        dumpTimeSecs = readWord dumpStream;
        dumpTimeMins = readWord dumpStream;
        dumpTimeHours = readWord dumpStream;
        dumpTimeDay = readWord dumpStream;
        dumpTimeMonth = readWord dumpStream;
        dumpTimeYear = readWord dumpStream;
    }

let mutable fsbBlob : byte [] = Array.zeroCreate 0
let mutable loadIt = false
let separator = "\\"
let mutable workingDir = ""

let processNameBlock ( options : CommandLineOptions ) ( blockLen : int) (reader : BinaryReader) : string = 
    let nameBytes = readBlob blockLen reader "Name"
    let fileName = (System.Text.Encoding.ASCII.GetString nameBytes).TrimEnd [| '\x00' |]
    if options.summary = WithSummary && options.verbose = VerboseOutput then    
        printfn ""
    let mutable fileType = ""
    let fstatType = int nameBytes.[1] |> enum
    let loadIt = 
        match fstatType with 
        | FSTATentryType.FLNK -> 
            let fileType = "=>Link=>"
            false
        | FSTATentryType.FDIR ->
            let fileType = "<Directory>"
            let workingDir = workingDir + separator + fileName
            //if options.extract = ExtractIt then
            // TODO
            false
        | FSTATentryType.FSTF ->
            let fileType = "Symbol Table"
            true
        | FSTATentryType.FTXT ->
            let fileType = "Text File"
            true
        | FSTATentryType.FPRG | FSTATentryType.FPRV ->
            let fileType = "Program File"
            true
        | _ -> // TODO we don't explicitly recognise the type - get definitive list from paru.32.sr
            let fileType = "file"
            true
    if options.summary = WithSummary then
        let displayPath = if workingDir = "" then fileName else workingDir + separator + fileName
        printf "%-12s: %-48s" fileType displayPath
        if options.verbose = VerboseOutput && fstatType = FSTATentryType.FDIR then
            printfn ""
        else
            printf "\t"
    // TODO extract and load code...
    fileName
    

let handleRecordHeader ( options : CommandLineOptions ) ( recHdr : recordHeader ) (reader : BinaryReader) : bool = 
    if options.verbose = VerboseOutput then
        printfn "Found block of type: %A length: %i" recHdr.recordType recHdr.recordLength
    match recHdr.recordType with
    | recordTypeT.START ->
        printfn "ERROR: Another START record found in DUMP - this should not happen"
        Environment.Exit 1
        false
    | recordTypeT.FSB ->
        let fsbBlob = readBlob recHdr.recordLength reader "FSB"
        false
    | recordTypeT.NB ->
        let fileName = processNameBlock options recHdr.recordLength reader
        true
    

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
            printfn "ERROR: No DUMP file specified, specify with --dumpfile <dumpfile> option"
            Environment.Exit 1
            1
        | _ -> 
            printfn "DEBUG: Processing DUMP file %s" options.dumpfilename
            let dfStream = File.OpenRead options.dumpfilename // TODO error handling
            use reader = new BinaryReader( dfStream )
            let bufferSize = 512
            
            // there should always be a SOD record...
            let sod = readSOD reader
            if options.summary =  WithSummary || options.verbose = VerboseOutput then
                printfn "Summary of DUMP file : %s" options.dumpfilename
                printfn "AOS/VS DUMP version  : %i" sod.dumpFormatRevision
                printfn "DUMP date (y-m-d)    : %i-%i-%i" sod.dumpTimeYear sod.dumpTimeMonth sod.dumpTimeDay
                printfn "DUMP time (hh:mm:ss) : %i:%i:%i" sod.dumpTimeHours sod.dumpTimeMins sod.dumpTimeSecs


            0 // return an integer exit code

