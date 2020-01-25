(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Demo1.fs: sample usage of engine
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.InteractiveMaze

open System
open Engine
open Gfx
open Maze
open System.Text
    

[< NoEquality; NoComparison >]
type state = {
    player : sprite
    sprites: sprite[]
}
let R = 20 //righe i
let C = 20  //colonne j
let GrandezzaCella=3
let centroCella = GrandezzaCella/2

let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next ())


let main () =       
    let engine = new engine (C*GrandezzaCella, R*GrandezzaCella)
    let maze = new maze(C,R,false)
    maze.generate
    let mutable vinto=false

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        if vinto=false then
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
            try //Controllo che la cella in cui voglio muovermi esiste, e che non esista un muro tra il player e la cella
                let fixedx = ((int x)-centroCella)/GrandezzaCella
                let fixedy = ((int y)-centroCella)/GrandezzaCella
                let currentX = ((int st.player.x)-centroCella)/GrandezzaCella
                let currentY = ((int st.player.y)-centroCella)/GrandezzaCella

                //Controllo che non ci sia un muro 
                let currentCella = maze.getByCoordinates(currentX,currentY)
                if(fixedx-currentX=1 && currentCella.rightWall=false || fixedx-currentX= -1 && currentCella.leftWall=false || fixedy-currentY=1 && currentCella.bottomWall=false || fixedy-currentY= -1 && currentCella.topWall=false) then
                    let cella = maze.getByCoordinates(fixedx,fixedy) 
                    cella.visited<-true
                    Log.msg "cella: (%d,%d), player: (%d,%d)" fixedx fixedy  currentX currentY
        
            
                    let NumeroMappato=fixedx+fixedy*C
                    if st.sprites.[NumeroMappato].z = 0 then //Solo quando non è mai stato sovrascritto
                        let immagine = image.cella (GrandezzaCella, GrandezzaCella, Color.Blue,cella)
                        st.sprites.[NumeroMappato].clear
                        st.sprites.[NumeroMappato]<-new sprite (immagine, int st.sprites.[NumeroMappato].x, int st.sprites.[NumeroMappato].y, int st.sprites.[NumeroMappato].z+1)
                        engine.register_sprite st.sprites.[NumeroMappato]
            
                    st.player.move_by (dx, dy)
                    if cella.finishLine=true then
                        Log.msg "HAI VINTO"
                        vinto<-true
            
            
            with 
            | :? System.IndexOutOfRangeException -> printfn "Exception handled.";
        st, key.KeyChar = 'q'

    let arr =[|
        for i in 0..R-1 do
            for j in 0..C-1 do
                if(i=0 && j=0) then maze.get(i,j).visited<-true;maze.get(i,j).topWall<-false
                let s = engine.create_and_register_sprite (image.cella (GrandezzaCella, GrandezzaCella, Color.Yellow,maze.get(i,j)), (GrandezzaCella*j),(GrandezzaCella*i), 0)
                yield s
    |]
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Blue), centroCella,centroCella, 0)

    engine.show_fps<-false
    let st0 = { 
        player = player
        sprites=arr
        }

    engine.loop_on_key my_update st0

