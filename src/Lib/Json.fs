namespace FSharpPractice

module Json =

    type PointInfo =
        { Names: Map<string, int>
          Point: Point }

    // in Newtonsoft.Json, json array can be mapped into a F# seq/array, but not F# list
    and Point = { X: int; Y: int; Lengths: int seq }

    open Newtonsoft.Json
    open Microsoft.VisualStudio.TestTools.UnitTesting

    // test the operate of serialize and deserialize
    let serializeJson = JsonConvert.SerializeObject
    let deserializeJson = JsonConvert.DeserializeObject<PointInfo>

    [<TestClass>]
    type Test() =

        [<TestMethod>]
        member _.TestJson() =
            let jsonText =
                serializeJson
                    { Names =
                          Map [ "F#", 1
                                "Haskell", 2
                                "Scala", 3 ]
                      Point = { X = 1; Y = 2; Lengths = [ 1; 2; 3 ] } }

            printfn <| Printf.TextWriterFormat<_> jsonText
            printfn "%A" <| deserializeJson jsonText
