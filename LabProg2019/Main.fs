﻿(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Main.fs: main code
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Main

open System
open System.Diagnostics
open Globals
open System.IO.Pipes
open System.IO


// game mode (client)
//
  
let main_game () =
    use p = new Process ()
    p.StartInfo.UseShellExecute <- true
    p.StartInfo.CreateNoWindow <- false
    p.StartInfo.Arguments <- "1"
    p.StartInfo.FileName <- Process.GetCurrentProcess().MainModule.FileName
    ignore <| p.Start ()

    use client = new NamedPipeClientStream (".", Config.log_pipe_name, PipeDirection.Out)
    client.Connect ()
    Log <- new remote_logger (client)

    // TODO: call your own main here
    Demo2.main ()
    0

// log mode (server)
//

let main_log_server () =
    Log.msg "log server process started"
    Console.Title <- Config.log_console_title
    let server = new NamedPipeServerStream (Config.log_pipe_name, PipeDirection.In)
    Log.msg "waiting for incoming connection..."
    server.WaitForConnection ()
    Log.msg "client connected"
    use r = new StreamReader (server)
    while not r.EndOfStream do
        try
            let fg = r.ReadLine ()
            let parse c = Enum.Parse (typeof<Color>, c) :?> Color
            let s = r.ReadLine().Replace (Config.log_pipe_translate_eol, '\n')
            Console.ForegroundColor <- parse fg
            Console.WriteLine s
            Console.ResetColor ()
        with e -> Log.error "exception caught:%s\nstack trace: %O" e.Message e

    Log.warn "EOF reached: quitting."
    0

// main 
//

[<EntryPoint>]
let main argv = 
    #if TEST
    Test.main ()
    printfn "\nPress any key to quit..."
    Console.ReadKey () |> ignore
    0
    #else
    if argv.Length > 0 then 
        main_log_server ()
    else
        let code = main_game ()
        printfn "\nPress any key to quit..."
        Console.ReadKey () |> ignore
        code
    #endif

