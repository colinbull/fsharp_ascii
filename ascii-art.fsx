open System.Drawing

type asciiLine = string list
type asciiArt  = asciiLine list

let asciiMap = [
    (230, " ");
    (200, ".");
    (180, ":");
    (160, "-");
    (130, "=");
    (100, "+");
    ( 70, "*");
    ( 50, "%");
    (  0, "@")
]

// Coordinate functions.

let coordinates maxX maxY = [
    for y in 0 .. maxY -> 
        [ for x in 0 .. maxX -> x, y ]
]

let bitmapCoordinates (bmp:Bitmap) =
    coordinates (bmp.Width - 1) (bmp.Height - 1)

// ASCII art functions

let toGreyScale (c:Color) =
    (int c.R + int c.G + int c.B) / 3

let toCharacter greyScale =
    List.find (fun (scale, _) -> greyScale >= scale) asciiMap
    |> snd

let processPixel (bmp:Bitmap) (x, y) =
    bmp.GetPixel(x, y)
    |> toGreyScale
    |> toCharacter

let toAsciiArt bmp =
    bmp
    |> bitmapCoordinates
    |> List.map (List.map (processPixel bmp))

// Output format functions

let formatLine (line:asciiLine) =
    System.String.Join("", line)

let toString (art:asciiArt) =
    System.String.Join("\r\n", List.map formatLine art)

// Fade functions.

let asciiChars = 
    List.unzip asciiMap 
    |> snd

let shift list = 
    " " :: list
    |> Seq.take (List.length list)
    |> Seq.toList

let rec repeat n f x =
    match n with
    | 0 -> x
    | n -> repeat (n - 1) f (f x)

let getFadeMap n =
    repeat n shift asciiChars
    |> List.zip asciiChars 

// Append functions

let (@@@) a b = 
    List.map2 (@) a b 

let fadeCell fadeMap cell =
    List.find (fun (ch, fadedCh) -> ch = cell) fadeMap
    |> snd

let fadeLine fadeMap line =
    List.map (fadeCell fadeMap) line
    
//
// Compositional Transforms
//

let upsideDown art =
    List.rev art
     
let flip art =
    List.map List.rev art
    
let mirror art =
    art @@@ flip art 

let reflect art =
    art @ upsideDown art

let fade art =
    List.mapi (fun i line -> fadeLine (getFadeMap (i / 10)) line) art

let fadeReflect (art:asciiArt) : asciiArt =
    art @ (art |> upsideDown |> fade)

let lighter n art =
    let fadeMap = getFadeMap n
    List.map (fadeLine fadeMap) art

let lighterReflect n art =
    art @ (art |> upsideDown |> lighter n)

// Art algorithm

let processBitmap transform bmp =
    bmp
    |> toAsciiArt
    |> transform
    |> toString

// IO

let getBitmap file =
    let img = Bitmap.FromFile(__SOURCE_DIRECTORY__ + @"\" + file)
    new Bitmap(img)
    
let writeFile path string = 
    System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + @"\" + path, string)    

// Bitmaps

let church = getBitmap "church.jpg"          
let turing = getBitmap "turing.jpg"                     

// Artwork
processBitmap id church |> writeFile "church-ascii.txt"       
processBitmap id turing |> writeFile "turing-ascii.txt"    
    

processBitmap (mirror << fadeReflect) church |> writeFile "church-transform.txt"    
processBitmap (fun a -> (a @@@ lighter 4 a @@@ lighter 6 a) |> lighterReflect 2) turing |> writeFile "turing-transform.txt"   

// for best results display in editor with exteme zoom out e.g. notepad++
