module FSharpPractice.Lang.ComputationExprssion

type CustomBuilder() =

    // record the method call of this compute expression
    let log method v =
        printfn "Call { %s }: %A" method v
        v

    // record the call stack of the Delay member
    let mutable count = 0

    member _.Zero() = log "Zero" []

    member _.Return v = log "Return" [ v ]

    // can't use free point style
    member _.ReturnFrom v = log "Return" v

    member _.Yield v = log "Yield" [ v ]

    member _.YieldFrom v = log "YieldFrom" v

    member _.Combine(v1, v2) =
        sprintf "Combine, left: %A, right: %A" v1 v2
        |> log
        <| List.append v1 v2

    member _.Delay f =
        let deploy = sprintf "Deploy%d" count
        printfn $"Start {deploy} ..." // record the start call stack
        count <- count + 1

        let v = log deploy <| f ()
        printfn $"End {deploy}" // record the end call stack
        count <- count - 1

        v

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Test() =

    [<TestMethod>]
    member _.TestComputationExprssion() =

        let builder = CustomBuilder()

        printfn "------------- Result1: %A ------------- \n"
        <| builder { "Test1" }

        printfn "------------- Result2: %A ------------- \n"
        <| builder {
            "Test2(1)"
            if false then "Test2(2)"
            yield! [ "Test2(3)" ]
           }

        printfn "------------- Result3: %A ------------- \n"
        <| builder {
            1
            return 2
            yield! [ 3; 4 ]
            return! []
            return! [ 5; 6 ]
            return 777
           }
