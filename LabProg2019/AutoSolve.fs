module LabProg2019.AutoSolve

open System
open Engine
open Gfx
open Maze    

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
    let mutable stack:(bool*cell) list = []
    let mutable next = false,new cell()
    let mutable i = 0
    let mutable j = 0
    let mutable current:(bool*cell) = (false,(maze.Struttura.[i,j]))
    getCell(current).visited<-true

    let solve (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =

        let playerX=st.player.x
        let playerY=st.player.y
        if vinto = false then //Se non ho ancora vinto devo trovare la strada
            let x = int (int playerX/GrandezzaCella)
            let y = int (int playerY/GrandezzaCella)

            if getBool(current)=false then
               j<-(getCell(current).x)
               i<-(getCell(current).y)
               
               next <- getPossiblePath(i,j,R,C,maze.Struttura)
               Log.msg "nextNode (%d,%d)" (getCell(next).x) (getCell(next).y)
               if getBool(next)=false then
                    
                   getCell(next).visited<-true
                   
                   let NumeroMappato=getCell(next).x+getCell(next).y*C
                   if st.sprites.[NumeroMappato].z = 0 then //Solo quando non è mai stato sovrascritto
                       st.player.x <- float ((getCell(next).x)*GrandezzaCella+centroCella)
                       st.player.y <- float ((getCell(next).y)*GrandezzaCella+centroCella)
                       let immagine = image.cella (GrandezzaCella, GrandezzaCella, Color.Blue,getCell(next))
                       st.sprites.[NumeroMappato].clear
                       st.sprites.[NumeroMappato]<-new sprite (immagine, int st.sprites.[NumeroMappato].x, int st.sprites.[NumeroMappato].y, int st.sprites.[NumeroMappato].z+1)
                       engine.register_sprite st.sprites.[NumeroMappato]
            
                   current<-next
                   stack<-(push(stack,current))
               else 
                   if stack.Length>0 then
                       
                       while(getBool(getPossiblePath(i,j,R,C,maze.Struttura))=true) do
                           current<-stack.Head
                           stack<-stack.Tail
                           j<-(getCell(current).x)
                           i<-(getCell(current).y)
            else 
                if stack.Length>0 then 
                    current<-stack.Head
                    stack<-stack.Tail
            
            //Check finale per vedere se si vince
            if ((maze.getByCoordinates(x,y).finishLine)=true) then
                vinto<-true
                st.player.x<-playerX
                st.player.y<-playerY
        st, match keyo with None -> false | Some k -> k.KeyChar = 'q'

    let arr =[|
        for i in 0..R-1 do
            for j in 0..C-1 do
                if(i=0 && j=0) then maze.get(i,j).visited<-true;maze.get(i,j).topWall<-false
                let s = engine.create_and_register_sprite (image.cella (GrandezzaCella, GrandezzaCella, Color.Yellow,maze.get(i,j)), (GrandezzaCella*j),(GrandezzaCella*i), 0)
                yield s
    |]
    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Blue), centroCella,centroCella, 3)

    engine.show_fps<-false
    let st0 = { 
        player = player
        sprites=arr
        }

    engine.loop solve st0
    
    
