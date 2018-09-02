#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open System.Xml
open FSharp.Data.Runtime.BaseTypes
#r "System.Xml.Linq.dll"
open FSharp.Data
open System.Xml.Linq
open System.IO
open System.Text
open System.Xml


let initialPwd = System.Environment.CurrentDirectory

isNull <| System.Environment.GetEnvironmentVariable "foo"

let vids = @"E:\launchbox-curation\amiga-curation\vids"
let roms = @"E:\launchbox-curation\amiga-curation\roms"

System.Environment.CurrentDirectory <- vids

let vidsSeq = System.IO.Directory.EnumerateFiles vids

let fixFileName (baseName: string): string =
    // slicing is range end inclusive in f#
    let underscorePos = baseName.IndexOf("_")
    if underscorePos <> -1 then
        let keepName = baseName.[0..underscorePos - 1]
        let ext = Path.GetExtension(baseName)
        let newName = keepName + ext
        newName
    else
        baseName


let oldNewNames (inputDirectory: string): seq<string * string> =
    System.IO.Directory.EnumerateFiles inputDirectory
    |> Seq.map (fun f -> (f, System.IO.Path.GetFileName(f)) )
    |> Seq.map (function (fullPath, baseName) -> (fullPath, fixFileName baseName))
    |> Seq.map (function (fullPath, fixedName) -> (fullPath, Path.Combine(Path.GetDirectoryName(fullPath), fixedName)))

let doRenames srcDstPaths =
    srcDstPaths
    |> Seq.filter (function (oldPath, newPath) -> oldPath <> newPath && not(File.Exists(newPath)))
    |> Seq.iter (function (oldPath, newPath) -> File.Move(oldPath, newPath))

oldNewNames roms |> doRenames
oldNewNames vids |> doRenames


// WHDLoad.xml processing
// FileName="WHDLoad\originalname.zip" -> FileName="WHDLoad\shortername.zip"
// or xml processing library, xpath
// Game.FileName attribute processing
// type provider?

[<Literal>]  // must be a constant expression for the type provider
let whdload_xml_file = @"E:\LaunchBox\Emulators\WinUAELoader\Data\WHDLoad.xml"
// let f = File.OpenRead(whdload_xml_file)
// let whdload_xml = File.ReadAllText(whdload_xml_file)
// type AuthorAlt = XmlProvider<"<author><name>Karl Popper</name><born>1902</born></author>">
type WHDLoad = XmlProvider<whdload_xml_file>

// A subclass of Linq XElement
// Hard to see type, dynamically genreated:
// XmlProvider<whdload_xml_file>.WhdLoad
let document = WHDLoad.Parse (File.ReadAllText(whdload_xml_file))
// document.Version
// document.NumGames
// document.XElement

let gameWithFileName (game: WHDLoad.Game) (fileName: string): WHDLoad.Game =
    // To mutate/update we cannot use the 'with' keyword - it's not a record
    // type Game inherits XmlElement and takes a single tuple arg constructor
    // the formal arg names are different cast to the property names: 'name' vs 'Name'
    new WHDLoad.Game (name=game.Name, fileName=fileName, scrnshotFilename=game.ScrnshotFilename,
                      useMouse=game.UseMouse, palNtsc=game.PalNtsc, cd=game.Cd, whdLoad=game.WhdLoad,
                      uaeConfig=game.UaeConfig)

let updatedWhdLoadXml (whdload_doc: XmlProvider<whdload_xml_file>.WhdLoad) : XElement =
    // Mutable way
    // Make whdload_doc.XElement is just the root node without all the Games
    // whdload_doc.XElement.RemoveNodes()
    // whdload_doc.XElement.Add(newGameDefs)

    // C# has the implicit string to XName conversion
    // let xn s =
    //     XName.Get(s)

    // We ToString and reparse the xml as the WHDLoad.Game constructor escapes the xml with &gt &lt
    // which looks ugly and doesn't pretty print.
    let newGameDefs = seq { for g in whdload_doc.Games -> gameWithFileName g (fixFileName g.FileName)
                                                          |> (fun g -> g.ToString())
                                                          |> XElement.Parse }

    // A new XmlElement can be constructed from a node name, attributes which apply to that element and
    // other elements which will be children
    // The C# constructor takes the var arg syntax Object [] as its last parameter
    // https://docs.microsoft.com/en-us/dotnet/api/system.xml.linq.xelement.-ctor?view=netframework-4.7.2#System_Xml_Linq_XElement__ctor_System_Xml_Linq_XName_System_Object___
    new XElement(whdload_doc.XElement.Name, whdload_doc.XElement.Attributes(), newGameDefs)

let newXml = updatedWhdLoadXml document
let [<Literal>] whd_out_file = @"E:\LaunchBox\Emulators\WinUAELoader\Data\WHDLoad_fileRenames.xml"
newXml.Save(whd_out_file)


let mmmap = [(1, 'a'); (2, 'b')] |> Map.ofList
