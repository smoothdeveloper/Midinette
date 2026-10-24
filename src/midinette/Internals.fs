module PrettyPrint
open System.Text

let printLines items formats (output: StringBuilder) =
  let textItems =
    seq {
      
      for title,getItemText in formats do
        yield title, items |> Array.map getItemText
    } |> dict

  let lines = Array.init (Array.length formats) StringBuilder
  let append (text: string) length (builder: StringBuilder) =
    builder.Append(text.PadLeft length) |> ignore
    builder.Append(" ") |> ignore
  let titles = formats |> Seq.map fst |> Seq.toArray
  let titleWidth = textItems.Keys |> Seq.map String.length |> Seq.max
  lines |> Array.iteri (fun i b -> append titles.[i] titleWidth b)
  for itemIndex in 0..items.Length - 1 do
    let _item = items.[itemIndex]
    let maxWidth = textItems.Values |> Seq.map (fun a -> Array.get a itemIndex) |> Seq.map String.length |> Seq.max
    lines |> Array.iteri (fun i b -> append textItems.[titles.[i]].[itemIndex] maxWidth b)
   
  for l in lines do
    output.AppendLine (string l) |> ignore
    
let printGrid items formats (output: StringBuilder) =
  let a (t:string) = output.Append t |> ignore
  let al (s: string) = output.AppendLine s |> ignore
  for title,_,width in formats do
    sprintf "%s\t" ((sprintf "%s" title).PadLeft(width, ' ')) |> a
  al ""

  for item in items do
    for title,format,width in formats do
      let width = if title.Length > width then title.Length else width
      sprintf "%s\t" (((format item): string).PadLeft(width, ' ')) |> a
    al ""
open FSharp.UMX
let printBytes (bytes: byte<_> array) =
  let mutable counter = 0
  let breakAt = 16
  let extraIndentAt = 4
  for b in bytes do
    if (counter % extraIndentAt) = 0 && counter <> 0 then printf " "
    printf "%02x " b
    if counter = (breakAt - 1) then printfn ""
    counter <- (counter + 1) % breakAt
  printfn ""

let sprintBits (n: byte) =
    Array.init 10 (function
        | 0 -> '0'
        | 1 -> 'b'
        | i when n >>> 9 - i &&& 1uy = 1uy -> '1'
        | _ -> '0'
    )
    |> System.String
let printBits (n: byte) =
    sprintBits n
    |> printfn "%s"

let printBytesDiff (b1: byte array) (b2: byte array) =
  
  for i in 0 .. (Array.length b1) - 1 do
    if b1.[i] <> b2.[i] then
      printfn "diff at %05i: %03i -> %03i" i b1.[i] b2.[i]


