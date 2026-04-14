namespace Lazy

type ILazy<'a> =
    abstract member Get: unit -> 'a

type LazySimple<'a>(supplier: unit -> 'a) =
    let mutable result: 'a option = None

    interface ILazy<'a> with
        member this.Get() =
            match result with
            | Some value -> value
            | None ->
                let value = supplier()
                result <- Some value
                value

type LazyThreadSafe<'a>(supplier: unit -> 'a) =
    let mutable result: 'a option = None
    let lockObj = obj()

    interface ILazy<'a> with
        member this.Get() =
            match result with
            | Some value -> value
            | None ->
                lock lockObj (fun () ->
                    match result with
                    | Some value -> value
                    | None ->
                        let value = supplier()
                        result <- Some value
                        value
                )

type LazyLockFree<'a>(supplier: unit -> 'a) =
    let mutable result: 'a option = None

    interface ILazy<'a> with
        member this.Get() =
            match result with
            | Some value -> value
            | None ->
                let value = supplier()
                let prev = System.Threading.Interlocked.CompareExchange(&result, Some value, None)
                match prev with
                | Some existing -> existing
                | None -> value