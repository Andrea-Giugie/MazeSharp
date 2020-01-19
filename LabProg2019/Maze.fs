(*
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

// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze(w , h ) =
    member val Struttura:cell[,] = Array2D.create h w (new cell())

    member this.get(x:int,y:int):cell= 
        Array2D.get this.Struttura x y
    member this.getByCoordinates(y:int,x:int):cell= 
        Array2D.get this.Struttura x y
     member this.generate=
         for i in 0..h-1 do
             for j in 0..w-1 do
                 this.Struttura.[i,j]<-new cell()
                     

        