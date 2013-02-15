module BresenAlgorithms

open System
open System.Collections.Generic
open System.Drawing

///Bresenham algorithm takes two points and a bitmap and draws a line on it
let inline bresenham (input : Bitmap) ((x0, y0) : int * int) ((x1, y1) : int * int) =
    let steep = abs (y1 - y0) > abs (x1 - x0)
    let x0, y0, x1, y1 =
        if steep then y0, x0, y1, x1 else x0, y0, x1, y1 //if it's steep, switch x and y for the algorithm
    let x0, y0, x1, y1 =
        if x0 > x1 then x1, y1, x0, y0 else x0, y0, x1, y1 //if the 2nd point comes before the 1st, swap them
    let (dx : float), (dy : float) = float (x1 - x0), float (abs (y1 - y0)) //create deltax and deltay
    let step = if y0 < y1 then 1 else -1
    let rec loop (e : float) (x : int) (y : int) =
        if x <= int x1 then
            if steep then input.SetPixel (y, x, Color.Black) else input.SetPixel (x, y, Color.Black)
            if e < dy then
                loop (e - dy + dx) (x + 1) (y + step)
            else
                loop (e - dy) (x + 1) y
    loop (dx / 2.0) (int x0)  (int y0) //start use loop
    input

///uses the midpoint circle algorithm to draw a rasterized circle on a bitmap given two coordinates, the first being the midpoint, the second is used to create the radius
let midpoint (input : Bitmap) ((x0, y0) : (int * int)) ((x1, y1) : (int * int)) =
    let radius = int (sqrt (abs ((float (x1 - x0)) ** 2.0 + (float (y1 - y0)) ** 2.0)))
    let corner = int (sqrt ((float radius ** 2.0) / 2.0))

    if x0 + corner < input.Width && y0 + corner < input.Height then input.SetPixel(x0 + corner, y0 + corner, Color.Black)
    if x0 - corner > 0 && y0 + corner < input.Height then input.SetPixel(x0 - corner, y0 + corner, Color.Black)
    if x0 - corner > 0 && y0 - corner > 0 then input.SetPixel(x0 - corner, y0 - corner, Color.Black)
    if x0 + corner < input.Width && y0 - corner > 0 then input.SetPixel(x0 + corner, y0 - corner, Color.Black)

    let rec loop (x, y) (ddf_x, ddf_y) f =
        if x < y then

            if x0 + x < input.Width && y0 + y < input.Height then input.SetPixel(x0 + x, y0 + y, Color.Black)//do every quadrant in one loop
            if x0 - x > 0 && y0 + y < input.Height then input.SetPixel(x0 - x, y0 + y, Color.Black)
            if x0 + x < input.Width && y0 - y > 0 then input.SetPixel(x0 + x, y0 - y, Color.Black)
            if x0 - x > 0 && y0 - y > 0 then input.SetPixel(x0 - x, y0 - y, Color.Black)
            if x0 + y < input.Width && y0 + x < input.Height then input.SetPixel(x0 + y, y0 + x, Color.Black)
            if x0 - y > 0 && y0 + x < input.Height then input.SetPixel(x0 - y, y0 + x, Color.Black)
            if x0 + y < input.Width && y0 - x > 0 then input.SetPixel(x0 + y, y0 - x, Color.Black)
            if x0 - y > 0 && y0 - x > 0 then input.SetPixel(x0 - y, y0 - x, Color.Black)

            if f >= 0 then loop (x, y - 1) (ddf_x, ddf_y + 2) (f + ddf_y + 2)//if moving faster along the y axis
            else loop (x + 1, y) (ddf_x + 2, ddf_y) (f + ddf_x + 2)
    loop (0, radius) (1, -2 * radius) (1 - radius) //first run of loop
    input

///returns a list of visible points given a destination and a startpoint
let bresenList (input : Bitmap) ((x0, y0) : int * int) ((x1, y1) : int * int) =
        let stepX = if x0 < x1 then 1 else -1 //step in the x direction
        let stepY = if y0 < y1 then 1 else -1
        let (dx : float), (dy : float) = float (abs (x1 - x0)), float (abs (y1 - y0)) //create deltax and deltay
        
        let rec loop (e : float) (x : int) (y : int) =
            [
                //let culurr1 = input.GetPixel (x, y)
                //let culurr2 = input.GetPixel (y, x)
                if (input.GetPixel(x,y).ToArgb () <> Color.Black.ToArgb ()) then //only runs if it hasn't reached a wall
                    if (x <> x1 || y <> y1) then //if it hasn't reached the end, keep going
                        yield (x, y) //draw
                        if (2.0 * e) > -dy then
                            if (2.0 * e) < dx then yield! loop (e - dy + dx) (x + stepX) (y + stepY) else yield! loop (e - dy) (x + stepX) (y)
                        else yield! loop (e + dx) (x) (y + stepY)
            ]
        loop (dx - dy) (int x0) (int y0) //start use loop

///returns a list of points of a circle
let listpoint (input : Bitmap) ((x0, y0) : (int * int)) radius =
    let corner = int (sqrt ((float radius ** 2.0) / 2.0))

    let corners = 
        [
            if x0 + corner < input.Width && y0 + corner < input.Height then yield (x0 + corner, y0 + corner)
            if x0 - corner > 0 && y0 + corner < input.Height then yield (x0 - corner, y0 + corner)
            if x0 - corner > 0 && y0 - corner > 0 then yield (x0 - corner, y0 - corner)
            if x0 + corner < input.Width && y0 - corner > 0 then yield (x0 + corner, y0 - corner)
        ]
    
    let rec loop (x, y) (ddf_x, ddf_y) f =
        [
            if x < y then

                if x0 + x < input.Width && y0 + y < input.Height then yield (x0 + x, y0 + y)//do every quadrant in one loop
                if x0 - x > 0 && y0 + y < input.Height then yield (x0 - x, y0 + y)
                if x0 + x < input.Width && y0 - y > 0 then yield (x0 + x, y0 - y)
                if x0 - x > 0 && y0 - y > 0 then yield (x0 - x, y0 - y)
                if x0 + y < input.Width && y0 + x < input.Height then yield (x0 + y, y0 + x)
                if x0 - y > 0 && y0 + x < input.Height then yield (x0 - y, y0 + x)
                if x0 + y < input.Width && y0 - x > 0 then yield (x0 + y, y0 - x)
                if x0 - y > 0 && y0 - x > 0 then yield (x0 - y, y0 - x)

                if f >= 0 then yield! loop (x, y - 1) (ddf_x, ddf_y + 2) (f + ddf_y + 2)//if moving faster along the y axis
                else yield! loop (x + 1, y) (ddf_x + 2, ddf_y) (f + ddf_x + 2)
         ]
    (loop (0, radius) (1, -2 * radius) (1 - radius)) @ corners //return the loop's points and the corners