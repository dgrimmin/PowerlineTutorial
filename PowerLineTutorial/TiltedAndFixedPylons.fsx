(*** hide ***)
//Powerline oriented programming example
//Dennis Grimminck, April 4th 2019
#load "../packages/FsLab/FsLab.fsx"
#load "../ReportUtils/ReportLayout.fsx"
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

//Physical constants
let [<Literal>] GravitationalAcceleration = 9.81<m/s^2>

//Simple types
type Height = Height of float<m>
type LinearWeight = LinearWeight of float<N/m>
type CatenaryConstant = private CatenaryConstant of float<m>
type Xcoordinate = Xcoordinate of float<m>

//Composed types
type Pylon =
    |FixedPylon of Xcoordinate
    |TiltedPylon of Xcoordinate*Xcoordinate

type PowerlineSpatialRepresentation =
    {
        pylonCoordinates:     (float<m> list*float<m> list) list  
        conductorCoordinates: (float<m> list*float<m> list) list
    }

type PowerlineSystem =
    private
        {
            pylons:       Pylon list
            spanlengths:  float<m> list
            height:       float<m>
            catenaryConstant: float<m>
            linearweight: float<N/m>
        }

//Enforce catenary constant construction
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CatenaryConstant =
    let init (catenaryConstant:float<m>) =
        if catenaryConstant <= 0.<m> then
            None
        else
            catenaryConstant |> CatenaryConstant |> Some 
    let value (CatenaryConstant c) = c 

//Enforce powerlinesystem construction
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PowerlineSystem =
    let init (Height overallHeight) (LinearWeight linearWeight) (pylons:Pylon list) (catenaryConstant:CatenaryConstant) =
        if pylons.Length > 2 then
            let spans =
                pylons
                |> List.pairwise 
                |> List.map (function
                             |FixedPylon (Xcoordinate x1), FixedPylon (Xcoordinate x2) -> x2 - x1
                             |FixedPylon (Xcoordinate x1), TiltedPylon (_,(Xcoordinate xt)) ->
                                xt - x1
                             |TiltedPylon (_,(Xcoordinate xt)), FixedPylon (Xcoordinate x1)->
                                x1 - xt
                             |TiltedPylon (_,(Xcoordinate xt)), 
                                TiltedPylon (_,(Xcoordinate xt')) ->
                                xt' - xt
                )
            if spans |> List.forall ((<) 0.<m>) then
                {
                    pylons          =pylons
                    spanlengths     =spans
                    height          =overallHeight
                    catenaryConstant=catenaryConstant |> CatenaryConstant.value
                    linearweight    =linearWeight
                } |> Some
            else
                None
        else
            None
    
    //Create spatial representation of pylons and the conductor
    let toSpatialRepresentation (psystem :PowerlineSystem) =
        let pylonCoordinates =
            psystem.pylons
            |> List.map (function
                         |FixedPylon (Xcoordinate x) -> [x;x],[0.<m>;psystem.height]
                         |TiltedPylon ((Xcoordinate xb),(Xcoordinate xt)) -> [xb;xt],[0.<m>;psystem.height]
            )
        let conductorCoordinates =
            (psystem.pylons|>List.take psystem.spanlengths.Length,psystem.spanlengths)
            ||> List.map2 (fun pylon span -> 
                let (xBeginSpan,span') =
                    match pylon with
                    |FixedPylon (Xcoordinate x1) -> x1,span
                    |TiltedPylon (_,(Xcoordinate xt)) -> xt,span
                let x = [0.<m> .. 0.1<m> .. span']
                let xcat = x |> List.map ((+) xBeginSpan)
                let ycat = 
                    let csh = x |> List.map (fun x -> ((x-span'*0.5)/psystem.catenaryConstant |> cosh))
                    let offsety = csh.Head
                    csh |> List.map (fun y -> psystem.catenaryConstant*(y-offsety)+psystem.height) 
                (xcat,ycat))
        {pylonCoordinates    = pylonCoordinates
         conductorCoordinates= conductorCoordinates}
        

//Create powerline system
(*** define: powerlineSetup ***)
module PowerlineSetup =
    let height       = 11.<m> |> Height
    let linearWeight = 1.<kg/m>*GravitationalAcceleration |> LinearWeight 
    let pylons =
        // [FixedPylon (Xcoordinate 0.<m>)
        //  FixedPylon (Xcoordinate 30.<m>)
        [TiltedPylon ((Xcoordinate 0.<m>),(Xcoordinate 1.<m>))
         TiltedPylon ((Xcoordinate 30.<m>),(Xcoordinate 28.<m>))
         FixedPylon (Xcoordinate 63.<m>)
         FixedPylon (Xcoordinate 93.<m>)
         FixedPylon (Xcoordinate 113.<m>)
        ]
    let powerline = 
        CatenaryConstant.init 30.<m>
        |> Option.bind (PowerlineSystem.init height linearWeight pylons)
        |> function
            |None -> failwith "Check for positive catenary constant, number of pylons >2, and pylon coordinates should be ascending."
            |Some powerlineSystem -> powerlineSystem 

(*** hide ***)
//Prepare the Scatter object per element of the powerline system    
module PlotPowerline =
    open XPlot.Plotly
    open PowerlineSetup

    let spatialRepresentation =
        powerline
        |>PowerlineSystem.toSpatialRepresentation

    let pylons =
        spatialRepresentation.pylonCoordinates
        |> List.map (fun (xcat,ycat) -> Scatter(x=xcat,y=ycat,line=Line(color="red")))
    let powerline =
        spatialRepresentation.conductorCoordinates
        |> List.map (fun (xcat,ycat) -> Scatter(x=xcat,y=ycat,line=Line(color="blue")))
    let plot =
        pylons@powerline
        |> Chart.Plot
        |> Chart.WithXTitle "Distance [m]"
        |> Chart.WithYTitle "Height [m]"
        |> Chart.WithTitle  "Our Powerline!"
        |> Chart.WithLegend false

(**
Powerline oriented programming
==========================
Set the parameters of the powerline.
*)
(*** include: powerlineSetup ***)


(**
Tadaah, our powerline!
*)

(*** include-value:PlotPowerline.plot ***)