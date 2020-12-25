namespace rec FSharpPractice.Lang

// In a recursive declaration group, 'open' declarations must come first in each moduleF# Compiler(3200)
open System.Runtime.InteropServices

// Recursive Type, need "namespace rec"
type A =
    | A of A
    | B of B

type B =
    | A of A
    | B of B

// Inerface
type IPrint =
    abstract Print: string option -> unit

// Class
type Print([<Optional; DefaultParameterValue(1)>] x: int, ?y: float) =

    do printfn "Init the class..."
    let mutable s = x // private field

    // property，custom define the get/set mthod，use a private field to save the content
    member _.X
        with get () = s
        and set v = s <- v

    // proerty, auto generate the get/set method
    member val Y = y |> Option.defaultValue 0.0 with get, set

    // impelement the interface
    interface IPrint with
        member _.Print prefix =
            printfn
                (Printf.TextWriterFormat<_>
                 <| match prefix with
                    | Some (p) -> "[" + p + "] X: %d, Y: %f"
                    | None -> "X: %d, Y: %f")
                x
            <| Option.defaultValue 1.0 y

    static member Show s = printfn $"Show text: {s}"

// Local level module
module Interface =

    open Microsoft.VisualStudio.TestTools.UnitTesting

    let iPrint (p: IPrint) s =
        p.Print(None)
        Print.Show s

    [<TestClass>]
    type Test() =

        let p = Print()

        [<TestMethod>]
        member _.TestInterface() =
            iPrint p "Prefix ..."
            (p :> IPrint).Print <| Some("Type Up Cast ...")

        [<TestMethod>]
        member _.TestProperty() = printfn $"Point X: {p.X} Y: {p.Y}"
