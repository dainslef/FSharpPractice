module FSharpPractice.Lib.JsonWithProvider

open FSharp.Data

// type provider generates the different types by the input JSON text
type PersonInfo = JsonProvider<"""{ "name":"Winne", "age": 67 }""">
type PersonInfos = JsonProvider<"""[{ "name":"Mr.Shithole", "age": 67 }]""">

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Test() =

    [<TestMethod>]
    member _.TestJsonProvider() =
        let personInfo =
            PersonInfo.Parse """{ "name": "Xitele", "age": 67, "sex": "unknown" }"""

        let personInfos =
            PersonInfos.Parse """[{ "name": "the Emperor QingFeng", "age": 67 }, { "name":"King Gesar", "age": 67 }]"""

        // the generated type has the auto-generate field to represent the json schema
        printfn $"{personInfo}"
        printfn $"Name: {personInfo.Name}, Age: {personInfo.Age}"
        printfn $"%A{personInfos}"

        for (i, info) in personInfos |> Array.indexed do
            printfn $"[{i}] Name: {info.Name}, Age: {info.Age}"
