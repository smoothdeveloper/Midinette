module Midinette.Tests
open Hopac
open Logary.Configuration
open Logary.Adapters.Facade
open System.IO
open Expecto
open Logary.Targets
open Midi
let tests =
    testList "midinette"
        [
            test "parse .scl" {
                let dotscl = """
! /Users/Shared/Scala/b6b7v3.scl
!
 b6b7 lin scale middle 69 A4 440 Bernard Durr 
 12
!
 77/72
 13/12
 7/6
 4/3
 49/36
 3/2
 14/9
 5/3
 7/4
 11/6
 35/18
 2/1
"""
                let result =
                    use reader = new StringReader(dotscl)
                    Tuning.Internals.SCLParser.parse reader
                printfn $"%A{result}"
                
                
                ()
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
