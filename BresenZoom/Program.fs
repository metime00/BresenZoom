module BresenZoom.Program
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.Windows.Forms
open Window

let win = new Window ()
win.Show()

while win.Visible do
    win.Refresh()
    Application.DoEvents ()