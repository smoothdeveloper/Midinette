// Learn more about F# at http://fsharp.org

open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Adapters.Facade
open PrettyPrint
open Elektron.MachineDrum
open System.IO
open System.Text
open Expecto
open Logary.Targets
open Midinette.Sysex
open Midi
open FSharp.UMX
let sysexFiles =
    let dir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data", "machinedrum"))
    dir.EnumerateFiles()
    |> Seq.map (fun f -> f, File.ReadAllBytes f.FullName)
let printBinary<[<Measure>] 'a> (value: byte<'a>) =
  let value = UMX.untag value
  let builder = StringBuilder()
  for i in 0 .. 7 do
    builder.Append (if ((value >>> i) &&& 0b1uy) = 1uy then "X" else ".") |> ignore
  builder.ToString()


let printTrigPattern (pattern: MDPattern) =
  let channelName = [
    "BD"; "SD"; "HT"; "MT" ;
    "LT"; "CP"; "RS"; "CB" ;
    "CH"; "OH"; "RC"; "CC" ;
    "M1"; "M2"; "M3"; "M3" ;
  ]
  for channel in 0 .. 15 do
    printf "%s\t" channelName.[channel]    
    let s = (channel * 4)
    let e = s + 3
    for b in pattern.TrigPattern.[s..e] do
      printf "%s" (printBinary b)
    if pattern.TrigPattern.Length > 64 then
      let s = (channel * 4) + 64
      let e = s + 3
      for b in pattern.TrigPattern.[s..e] do
        printf "%s" (printBinary b)
    printfn ""
    
    

let tests =
    testList "midinette"
        [
            test "parse sysex" {
                
                for _f, data in sysexFiles do
                    let messages = Sysex.getSysexMessagesFromBytes data
                    let indexedMessages =
                        messages 
                        |> Array.groupBy (fun m -> LanguagePrimitives.EnumOfValue (UMX.untag_sysex m.[0x06]) : MachineDrumSysexMessageId) 
                        |> Array.map (fun (k, vs) -> k, vs)
                        |> dict
                        
                    for k in indexedMessages.[MachineDrumSysexMessageId.Kit] do
                        let kit = MDKit.fromSysex k
                        printfn "parsed kit %i %s" kit.Position kit.Name                      
                        printfn "\t - drum models: %A" kit.SelectedDrumModel
                        printfn "\t - compressor: %A" kit.CompressorSettings
                        let actual = MDKit.toSysex kit
                        let arrays = Seq.zip actual k |> Seq.indexed
                        
                        for i , (a,b) in arrays do
                            if a <> b then
                              printBytes (actual.[i ..] )
                              printBytes k.[i..]
                            Expect.equal a b (sprintf "roundtrip of kit ends up being different at %i %i <> %i" i a b)
                        Expect.equal actual.Length k.Length "kit sysex has unexpected length"
                    for p in indexedMessages.[MachineDrumSysexMessageId.Pattern] do
                        let pattern = MDPattern.fromSysex p
                        printfn "parsed pattern %i (%i steps)" pattern.OriginalPosition pattern.NumberOfSteps
                        printTrigPattern pattern
                    for p in indexedMessages.[MachineDrumSysexMessageId.Global] do
                        let globalSettings = GlobalSettings.fromSysex p
                        printfn "parsed global settings %i" globalSettings.OriginalPosition 
                        printfn "\t - keymap: %A" globalSettings.KeymapStructure
                
            }
        ]
[<EntryPoint>]
let main args =
    let logary =
        Config.create "MyProject.Tests" "localhost"
        |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
        |> Config.processing (Events.events |> Events.sink ["console";])
        |> Config.build
        |> run
    LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

    
    runTestsWithArgs defaultConfig args tests
