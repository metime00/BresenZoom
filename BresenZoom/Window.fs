module Window

//open OpenTK
//open OpenTK.Graphics
//open OpenTK.Graphics.OpenGL
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open BresenAlgorithms
open Origin


type Window () as this =
    inherit Form ()

    ///the memoization of line of sight
    let mutable memo = Array2D.create 0 0 Array.empty
    ///the x offset of the bounding box
    let mutable xoffset = 0
    ///the y offset of the bounding box
    let mutable yoffset = 0

    ///resolution of the form
    let res = 50.0f
    ///LoS radius constant
    let field = 100

    ///takes a mouse coordinate and converts it into a scaled coordinate for the current bitmap resolution
    let coord mouse = int ((float32 (float mouse) / 96.0f) * res)

    let mutable (origin : Origin) = new Origin (0, 0, field) //not meant to be used

    ///original X coord
    let mutable x = 0
    ///original Y coord
    let mutable y = 0

    ///if the left mouse button is down, handles general drawing
    let mutable downLeft = false
    ///if right mouse button is down, handles supplementary drawing
    let mutable downRight = false
    ///if alt is down, handles squiggly lines
    let mutable alt = false
    ///if ctrl is down, handles raytracing functionality
    let mutable ctrl = false

    do
        this.Text <- "PubarZoom"
        this.Width <- 1000
        this.Height <- 511
        this.Cursor <- Cursors.Cross
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.White

    ///main bitmap
    let mutable (mit : Bitmap) =
        let tmp = new System.Drawing.Bitmap (coord this.Width, coord this.Height)
        tmp.SetResolution (res, res)
        tmp

    ///returns a 2d array of all the fields of vision in the bitmap, for fast calculating
    let memoize (input : Bitmap) =
        let mutable left = input.Width
        let mutable right = 0
        let mutable top = 0
        let mutable bottom = input.Height
        //very inefficiently finds the bounding box of the place to do calculations for
        for i = 0 to input.Width - 1 do
            for j = 0 to input.Height - 1 do
                if input.GetPixel(i, j).ToArgb () = Color.Black.ToArgb () then
                    if i < left then left <- i
                    if i > right then right <- i
                    if j < bottom then bottom <- j
                    if j > top then top <- j
        let mutable output = Array2D.create (right - left) (top - bottom) Array.empty
        xoffset <- left
        yoffset <- bottom

        for i = 0 to (Array2D.length1 output) - 1 do
            for j = 0 to (Array2D.length2 output) - 1 do
                //only do the calculations if the pixel isn't black, optimization
                if input.GetPixel(i + xoffset, j + yoffset).ToArgb () <> Color.Black.ToArgb () then output.[i,j] <- [|for (a, b) in listpoint input (i + xoffset, j + yoffset) origin.Size do yield! bresenList input (i + xoffset, j + yoffset) (a, b)|]
        output

    override this.OnPaint args =
        if mit <> null then args.Graphics.DrawImageUnscaled (mit, 0, 0)

    override this.OnMouseDown args =
        match args.Button with
        | MouseButtons.Right ->
            if args.X > 0 && args.Y > 0 then
                x <- coord args.X
                y <- coord args.Y
                downRight <- true
        | MouseButtons.Middle ->
            if ctrl then memo <- memoize mit
            else
                mit <- new System.Drawing.Bitmap (coord this.Width, coord this.Height)
                mit.SetResolution (res, res)//clears the screen
        | MouseButtons.Left -> 
            if args.X > 0 && args.Y > 0 then
                x <- coord args.X
                y <- coord args.Y
                downLeft <- true
        | _ -> ()

    override this.OnMouseUp args =
        if downLeft then
            if not alt && not ctrl then mit <- bresenham mit (x, y) (coord args.X, coord args.Y)//draw a basic line
            downLeft <- false
        if downRight then
            if ctrl then
                mit <- origin.clean mit
                origin <- new Origin(coord args.X, coord args.Y, field)//set the origin and its sight radius
                mit <- origin.drawRadius mit //draws the radius
            if not alt && not ctrl then mit <- midpoint mit (x, y) (coord args.X, coord args.Y)//draws a basic circle
            downRight <- false

    override this.OnMouseMove args =
        //if ctrl && origin.Size <> -1 then mit <- origin.drawLoS mit (coord args.X, coord args.Y)//draws line of sight from the origin to the cursor
        if downLeft && alt then
            if abs ((coord args.X) - x) > 1 || abs ((coord args.Y) - y) > 1 then mit <- bresenham mit (x, y) (coord args.X, coord args.Y)//free drawing
            else mit.SetPixel(coord args.X, coord args.Y, Color.Black)
            x <- coord args.X
            y <- coord args.Y
        if ctrl && (Array2D.length1 memo > 0) then//draws the line of sight using the memo
            mit <- origin.clean mit
            if ((coord args.X) - xoffset > 0 && (coord args.X) - xoffset < Array2D.length1 memo) && ((coord args.Y) - yoffset > 0 && (coord args.Y) - yoffset < Array2D.length2 memo) then
                mit <- origin.memoDraw mit memo.[(coord args.X) - xoffset, (coord args.Y) - yoffset] //draws the radius

    override this.OnKeyDown args =
        if args.Alt then alt <- true
        if args.Control then ctrl <- true

    override this.OnKeyUp args =
        if not (args.Alt) then alt <- false
        if not (args.Control) then
            mit <- origin.clean mit//cleans line of sight if control not held
            ctrl <- false