﻿(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx
open System
open System.Text



[<Diagnostics.DebuggerDisplay("{ ToString ()}")>]


type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall

(*let getRandomVicino(x:int,y:int,maxX:int,maxY:int,matrice:cell[,])=
    
    let l:cell[]=Array.create 4 (new cell())
    let mutable i = 0
    if((x-1)>=0 && (x-1)<maxX && y>=0 && y<maxY ) then l.[i]<-(matrice.[x-1,y]);i<-i+1   //Sinistra
    if((x+1)>=0 && (x+1)<maxX && y>=0 && y<maxY && (matrice.[x+1,y].visited=false)) then l.[i]<-(matrice.[x+1,y]);i<-i+1     //Destra
    if((x)>=0 && x<maxX && (y-1)>=0 && (y-1)<maxY && (matrice.[x,y-1].visited=false)) then l.[i]<-(matrice.[x,y-1]);i<-i+1     //Sopra
    if((x)>=0 && x<maxX && (y+1)>=0 && (y+1)<maxY && (matrice.[x,y+1].visited=false)) then l.[i]<-(matrice.[x,y+1]);i<-i+1     //Sotto
 
    l.[i]*)

let getRandomVicino(x:int,y:int,maxX:int,maxY:int,matrice:cell[,])=
    let mutable i = 0
    let l =[|
        if((x-1)>=0 && (x-1)<maxX && y>=0 && y<maxY && (matrice.[x-1,y].visited=false) ) then yield false,(matrice.[x-1,y]);i<-i+1   //Sinistra
        if((x+1)>=0 && (x+1)<maxX && y>=0 && y<maxY && (matrice.[x+1,y].visited=false)) then yield false,(matrice.[x+1,y]);i<-i+1     //Destra
        if((x)>=0 && x<maxX && (y-1)>=0 && (y-1)<maxY && (matrice.[x,y-1].visited=false)) then yield false,(matrice.[x,y-1]);i<-i+1     //Sopra
        if((x)>=0 && x<maxX && (y+1)>=0 && (y+1)<maxY && (matrice.[x,y+1].visited=false)) then yield false,(matrice.[x,y+1]);i<-i+1     //Sotto
    |]
    let r = System.Random().Next(0, i)
    if i = 0 then 
        (true, (new cell()))
    else l.[r]
let getBool((b,c):bool*cell)=b
let getCell((b,c):bool*cell)=c
let cancellaMuri(c1:cell,c2:cell)=
    let Deltax = c1.x-c2.x
    let Deltay = c1.y-c2.y
    if Deltax = 1 then
        c1.leftWall<-false
        c2.rightWall<-false
    if Deltax = -1 then
        c1.rightWall<-false
        c2.leftWall<-false
    if Deltay = -1 then
        c1.bottomWall<-false
        c2.topWall<-false
    if Deltay = 1 then
        c1.topWall<-false
        c2.bottomWall<-false
// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze(w , h ) =
    member val Struttura:cell[,] = Array2D.create h w (new cell())

    member this.get(x:int,y:int):cell= 
        Array2D.get this.Struttura x y
    member this.getByCoordinates(y:int,x:int):cell= 
        Array2D.get this.Struttura x y
    member this.generate=
     //Inizializzo la matrice
         for i in 0..h-1 do 
             for j in 0..w-1 do
                 this.Struttura.[i,j]<-new cell()
                 this.Struttura.[i,j].x<-j
                 this.Struttura.[i,j].y<-i
         this.Struttura.[0,0].visited<-true
         this.Struttura.[0,0].topWall<-false
         let mutable i = 0
         let mutable j = 0
         let mutable current = getRandomVicino(i,j,h,w,this.Struttura)
         cancellaMuri(getCell(current),this.Struttura.[0,0])
         let mutable next = false,new cell()
         getCell(current).visited<-true
         while getBool(current)=false do
            j<-(getCell(current).x)
            i<-(getCell(current).y)
            Log.msg "Exploring node: (%d,%d)" j i
            next <- getRandomVicino(i,j,h,w,this.Struttura)
            getCell(next).visited<-true
            //apro i muri
            cancellaMuri(getCell(next),getCell(current))
            
            current<-next
            
            
     //La prima cella e' visitata sicuramente
         
     //Apro i muri

                     

        