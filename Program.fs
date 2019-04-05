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
