(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Demo1.fs: sample usage of engine
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Demo2

open System
open Engine
open Gfx
open Maze
open System.Text
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
    maze: maze

}
let R = 20  //righe i
let C = 30  //colonne j


let main () =       
    let engine = new engine (C, R)
    let maze = new maze(C,R)
    maze.generate
         


    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        // TODO: check bounds
        let x = (st.player.x + float dx) 
        let y = (st.player.y + float dy)
        try
            let cella = st.maze.getByCoordinates((int x),(int y)) 
            
            Log.msg "%s" (cella.ToString())
            st.player.move_by (dx, dy)
            Log.msg "x: %f, y: %f" st.player.x st.player.y
            
        with 
        | :? System.IndexOutOfRangeException -> printfn "Exception handled.";
        st, key.KeyChar = 'q'

    // create simple backgroud and player
    //ignore <| engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 0, 0)
    for i in 0..R-1 do
        for j in 0..C-1 do
            if(maze.get(i,j).topWall=true) then
                ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Yellow), j-1,i, 0)
            if(maze.get(i,j).bottomWall=true) then
                ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Yellow), j+1,i, 0)
            if(maze.get(i,j).rightWall=true) then
                ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Yellow), j,i-1, 0)
            if(maze.get(i,j).leftWall=true) then
                ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Yellow), j,i+1, 0)

    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Blue), R/2,C/2, 0)
    engine.show_fps<-false
    // initialize state
    let st0 = { 
        player = player
        maze= maze
        }
    // start engine
    engine.loop_on_key my_update st0

