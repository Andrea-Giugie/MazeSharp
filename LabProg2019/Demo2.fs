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
    sprites: sprite[]

}
let R = 10  //righe i
let C = 15  //colonne j
let GrandezzaCella=5
let centroCella = GrandezzaCella/2

let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next ())


let main () =       
    let engine = new engine (C*GrandezzaCella, R*GrandezzaCella)
    let maze = new maze(C,R)
    maze.generate

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
        //let mutable nuovoSprite = st.sprites
        try
            let fixedx = ((int x)-centroCella)/GrandezzaCella
            let fixedy = ((int y)-centroCella)/GrandezzaCella
            let cella = st.maze.getByCoordinates(fixedx,fixedy) 
            cella.visited<-true
        
            //ignore <| engine.create_and_register_sprite (image.cella (GrandezzaCella, GrandezzaCella, Color.Blue,cella), (0),(0), 1 , 1)
            
            let NumeroMappato=fixedx+fixedy*C
            if st.sprites.[NumeroMappato].z = 0 then //Solo quando non è mai stato sovrascritto
                let immagine = image.cella (GrandezzaCella, GrandezzaCella, Color.Blue,cella)
                st.sprites.[NumeroMappato].clear
                st.sprites.[NumeroMappato]<-new sprite (immagine, int st.sprites.[NumeroMappato].x, int st.sprites.[NumeroMappato].y, int st.sprites.[NumeroMappato].z+1)
                engine.register_sprite st.sprites.[NumeroMappato]
            
            Log.msg "currentCell: (%d,%d):%s " fixedx fixedy ((st.maze.getByCoordinates(fixedx,fixedy)).ToString())
            //Log.msg "origin (0,0): %s "((st.maze.getByCoordinates(0,0)).ToString())
            
            //nuovoSprite<-new sprite (image.cella (GrandezzaCella, GrandezzaCella, Color.Blue,cella), int st.sprites.x, int st.sprites.y, 2,int st.sprites.id)
            //st.sprites<-nuovoSprite
            //engine.overrideSprite(nuovoSprite,st.sprites.id)
            //Log.msg "idPLayer: %d idCella: %d" st.player.id st.sprites.id 
            
            //st.sprites.move_by(1,0)
            //Log.msg "current: (%f,%f), going to: (%f,%f) maze: (%d,%d)" st.player.x st.player.y x y fixedy fixedx
            st.player.move_by (dx, dy)
            Log.msg "x: %f, y: %f" st.player.x st.player.y
            
            
        with 
        | :? System.IndexOutOfRangeException -> printfn "Exception handled.";
        st, key.KeyChar = 'q'

    // create simple backgroud and player
    //ignore <| engine.create_and_register_sprite (image.rectangle (W, H, pixel.filled Color.Yellow, pixel.filled Color.Blue), 0, 0, 0)
    let arr =[|
        for i in 0..R-1 do
            for j in 0..C-1 do
                if(i=0 && j=0) then maze.get(i,j).visited<-true;maze.get(i,j).topWall<-false
                let s = engine.create_and_register_sprite (image.cella (GrandezzaCella, GrandezzaCella, Color.Yellow,maze.get(i,j)), (GrandezzaCella*j),(GrandezzaCella*i), 0)
                yield s
    |]
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Blue), centroCella,centroCella, 0)

    engine.show_fps<-false
    // initialize state
    let st0 = { 
        player = player
        maze= maze
        sprites=arr
        }
    // start engine
    engine.loop_on_key my_update st0
