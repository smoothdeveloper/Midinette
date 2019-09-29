// Learn more about F# at http://fsharp.org

open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Adapters.Facade

open Expecto
open Logary.Targets

let tests =
    testList "midinette"
        [
            test "simple" {
                let p = Midinette.Platform.RtMidi.RtMidiMidinettePlatformImpl()
                for i, device in p.InputDevices |> Seq.indexed do
                    Expect.isNotNull (box device ) "devicenotnull"
                    printfn "input [%i] %A" i device
                for i, device in p.OutputDevices |> Seq.indexed do
                    printfn "output [%i] %A" i device
                
                
                
                () //Expecto.equal "" ""
            }
            
            test "loopback" {
                let p = Midinette.Platform.RtMidi.RtMidiMidinettePlatformImpl()
                for i, device in p.InputDevices |> Seq.indexed do
                    Expect.isNotNull (box device ) "devicenotnull"
                    printfn "input [%i] %A" i device
                for i, device in p.OutputDevices |> Seq.indexed do
                    printfn "output [%i] %A" i device

                
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
