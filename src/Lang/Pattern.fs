namespace FSharpPractice

module Pattern =

    type Point(?a: int, ?b: int, ?c: string) =
        member val X = a |> Option.defaultValue 0 with get, set
        member val Y = b |> Option.defaultValue 0 with get, set
        member _.Message = c |> Option.defaultValue ""

    // the grammar "(||)" called "banana clips", use to define the pattern,
    // the return value in function should be the defined pattern in this banana clips
    // the function created by this type of let binding is called an active recognizer
    let (|Pattern1|Pattern2|Pattern3|) (p: Point) =
        if p.X < 0 then Pattern1(p.X)
        elif p.Y < 0 then Pattern2(p.Y)
        else Pattern3

    // define a partial active pattern, you use a wildcard character (_) at the end of the list of patterns inside the banana clips
    // the return value of the function should be type "option"
    let (|PartialPattern1|_|) (p: Point) =
        if p.X > 0 && p.Y > 0 then Some((p.X, p.Y)) else None

    let (|PartialPattern2|_|) (p: Point) =
        if not <| isNull p.Message && p.Message.Length <> 0
        then Some(p.Message)
        else None

    // Parameterized Active Patterns
    let (|ParameterizedPattern|_|) ((x: int, y: int) as v) (p: Point) =
        if p.X = x && p.Y = y then Some((v, p)) else None

    // use the custom patterns
    let printPoint (p: Point) =
        match p with
        | ParameterizedPattern (10, 10) p -> printfn "ParameterizedPattern: Value: %A" p
        | PartialPattern1 (x, y) -> printfn "PartialPattern1: X: %d, Y: %d" x y
        | PartialPattern2 message -> printfn "PartialPattern2: Message: %s" message
        | Pattern1 v
        | Pattern2 v -> printfn "Pattern1|Patterns2: Value: %d" v
        | Pattern3 -> printfn "Pattern3"

    open Microsoft.VisualStudio.TestTools.UnitTesting

    [<TestClass>]
    type Test() =

        [<TestMethod>]
        member _.TestPattern() =
            [ Point(10, 10)
              Point(1, 1)
              Point(-1, -1, c = "Test")
              Point(-1, 2)
              Point(2, -1)
              Point() ]
            |> List.map printPoint
            |> ignore

        [<TestMethod>]
        [<DynamicData("Data", DynamicDataSourceType.Property)>]
        // DynamicDataSource類型需要為 IEnumerable<obj []>，將待測試方法的參數放在 obj [] 中
        member _.TestPattern1(p: Point) = printPoint p |> ignore

        static member Data =
            seq {
                Point(10, 10)
                Point(1, 1)
                Point(-1, -1, c = "Test")
                Point(-1, 2)
                Point(2, -1)
                Point()
            }
            |> Seq.map (fun v -> [| v :> obj |]) // seq 類型實現了IEnumerable接口

        [<TestMethod>]
        [<DataRow(10, 10)>]
        [<DataRow(1, 1)>]
        [<DataRow(-1, -1)>]
        [<DataRow(-1, 2)>]
        [<DataRow(2, -1)>]
        member this.TestPattern2(x: int, y: int) = this.TestPattern3(x, y, "")

        [<TestMethod>]
        [<DataRow(-1, -1, "Test")>]
        member this.TestPattern3(x: int, y: int, message: string) =
            this.TestPattern1 <| Point(x, y, message)
