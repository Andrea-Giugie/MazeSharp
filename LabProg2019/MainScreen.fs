module LabProg2019.MainScreen

open System
open Engine
open Gfx


let R = 20 //righe i
let C = 20  //colonne j
let GrandezzaCella=3

[< NoEquality; NoComparison >]
type state = {
    player:sprite
}

let main () =       
    let engine = new engine (C*GrandezzaCella, R*GrandezzaCella)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let tasto=
            match key.KeyChar with
            |'1'->1
            |'0'->0
            | _ -> -1

        if(tasto=1) then 
            InteractiveMaze.main()
        else if(tasto=0) then
                AutoSolve.main()                
        st, key.KeyChar = 'q'

    engine.show_fps<-false
    let sfondo = engine.create_and_register_sprite (image.rectangle (C*GrandezzaCella, R*GrandezzaCella, pixel.filled Color.Black, pixel.filled Color.Black), 0,0, 0)
    sfondo.draw_text("Premi 0 per risolvere automaticamente ",1,1,Color.White)
    sfondo.draw_text("Premi 1 per Giocare ",1,2,Color.White)
    let st={player=sfondo}
    engine.loop_on_key my_update st
