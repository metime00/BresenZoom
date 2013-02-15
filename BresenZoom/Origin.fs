module Origin

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open BresenAlgorithms

///handles bitmap drawing for line of sight, storage of currently visible points and all other line of sight operations
type Origin (x : int, y : int, size : int) =
    ///X coord of the point of origin, used for line of sight
    let mutable pX = x
    ///Y coord of the point of origin, used for line of sight
    let mutable pY = y
    ///array of currently visible pixels
    let mutable visiblePixels = Array.empty

    let rec except a b =
        match (a, b) with
        | [], x | x, [] -> x
        | x::xs, y::ys ->
            if x < y then x :: except xs (y::ys)
            elif x > y then y :: except (x::xs) ys
            else except xs ys

    ///takes a bitmap and a point and redraws the line of sight, returning the bitmap, and only redrawing the pixels where their visibility state has changed
    member this.drawLoS (input : Bitmap) ((x1, y1) : int * int) =
        for (a, b) in visiblePixels do
            if input.GetPixel(a, b).ToArgb () = Color.Blue.ToArgb () then input.SetPixel(a, b, Color.White)
        let newPoints = bresenList input (pX, pY) (x1, y1)
        for (a, b) in newPoints do
            input.SetPixel(a, b, Color.Blue)
        visiblePixels <- List.toArray newPoints
        input

    member this.drawRadius (input : Bitmap) =
        visiblePixels <- [|for (a, b) in listpoint input (pX, pY) size do yield! bresenList input (pX, pY) (a, b)|]
        for (a, b) in visiblePixels do
            input.SetPixel(a, b, Color.Blue)
        input

    ///takes a bitmap and returns the bitmap cleaned of all LoS pixels
    member this.clean (input : Bitmap) =
        for (a, b) in visiblePixels do
            if input.GetPixel(a, b).ToArgb () = Color.Blue.ToArgb () then input.SetPixel(a, b, Color.White)
        input

    ///draws the line of sight using a memo
    member this.memoDraw (input : Bitmap) (pixels : (int * int)[]) =
        visiblePixels <- pixels
        for (a, b) in visiblePixels do
            input.SetPixel(a, b, Color.Blue)
        input

    ///X coord of the vision point of origin
    member this.X
        with get() = pX
        and set value = pX <- value
    ///Y coord of the vision point of origin
    member this.Y
        with get() = pY
        and set value = pY <- value
    ///width of the vision field, it looks like a laser
    member this.Size
        with get() = size

    
