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
let R = 10  //righe i
let C = 15  //colonne j
let GrandezzaCella=7
let centroCella = GrandezzaCella/2




let main () =       
    let engine = new engine (C*GrandezzaCella, R*GrandezzaCella)
    let maze = new maze(C,R)
    maze.generate
    
    let disegna()=
        for i in 0..R-1 do
            for j in 0..C-1 do
                ignore <| engine.create_and_register_sprite (image.cella (GrandezzaCella, GrandezzaCella, Color.Yellow,maze.get(i,j)), (GrandezzaCella*j),(GrandezzaCella*i), 0)


    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., float -GrandezzaCella
            | 's' -> 0., float GrandezzaCella
            | 'a' -> float -GrandezzaCella, 0.
            | 'd' -> float GrandezzaCella,0.
            | _   -> 0., 0.
        // TODO: check bounds
        let x = (st.player.x + float dx) 
        let y = (st.player.y + float dy)
        try
            let fixedx = ((int x)-centroCella)/GrandezzaCella
            let fixedy = ((int y)-centroCella)/GrandezzaCella
            let cella = st.maze.getByCoordinates(fixedx,fixedy) 
            cella.visited<-true
            
            Log.msg "current: (%f,%f), going to: (%f,%f) maze: (%d,%d)" st.player.x st.player.y x y fixedy fixedx
            st.player.move_by (dx, dy)
            Log.msg "x: %f, y: %f" st.player.x st.player.y
            
        with 
        | :? System.IndexOutOfRangeException -> printfn "Exception handled.";
        
        st, key.KeyChar = 'q'

    // create simple backgroud and player
    //ignore <| engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 0, 0)

    disegna()   
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Blue), centroCella,centroCella, 0)
    engine.show_fps<-false
    // initialize state
    let st0 = { 
        player = player
        maze= maze
        }
    // start engine
    engine.loop_on_key my_update st0

