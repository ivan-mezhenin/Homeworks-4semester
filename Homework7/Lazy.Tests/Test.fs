module LazyTests

open NUnit.Framework
open FsUnit
open Lazy

let private testLazyOnce (createLazy: (unit -> 'a) -> ILazy<'a>) =
    let mutable callCount = 0

    let supplier () =
        callCount <- callCount + 1
        42

    let lazyObj = createLazy supplier

    lazyObj.Get() |> should equal 42
    callCount |> should equal 1

    lazyObj.Get() |> should equal 42
    lazyObj.Get() |> should equal 42

    callCount |> should equal 1

[<Test>]
let ``LazySimple - supplier is called only once`` () =
    testLazyOnce (fun supplier -> LazySimple(supplier) :> ILazy<int>)

[<Test>]
let ``LazyThreadSafe - supplier is called only once`` () =
    testLazyOnce (fun supplier -> LazyThreadSafe(supplier) :> ILazy<int>)

[<Test>]
let ``LazyLockFree - always returns the same value (supplier may be called multiple times)`` () =
    let mutable callCount = 0

    let supplier () =
        callCount <- callCount + 1
        System.Guid.NewGuid()

    let lazyObj = LazyLockFree(supplier) :> ILazy<System.Guid>

    let firstResult = lazyObj.Get()

    for _ in 1..5 do
        lazyObj.Get() |> should equal firstResult

    callCount |> should be (greaterThan 0)

[<Test>]
let ``All lazy implementations support null value`` () =
    let testNull (create: (unit -> string) -> ILazy<string>) =
        let lazyObj = create (fun () -> null)
        lazyObj.Get() |> should be Null
        lazyObj.Get() |> should be Null

    testNull (fun s -> LazySimple(s) :> ILazy<string>)
    testNull (fun s -> LazyThreadSafe(s) :> ILazy<string>)
    testNull (fun s -> LazyLockFree(s) :> ILazy<string>)
