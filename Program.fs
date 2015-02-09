open System.Threading.Tasks

////////////////////////////////////////////////////////////////////////

module Pure =
    type Instances = Instances with
        static member inline Pure(_: 'a option) = fun x -> Some x
        static member inline Pure(_: 'a list)   = fun x -> [x]
        static member inline Pure(_: 'a seq)    = fun x -> Seq.singleton x
        static member inline Pure(_: 'a array)  = fun x -> [|x|]
        static member inline Pure(_: 'a Async)  = fun x -> async.Return (x : 'a)
        static member inline Pure(_: 'a Task)   = fun x -> Task.FromResult (x : 'a)

    let inline instance (a: ^a, b: ^b) =
        ((^a or ^b) : (static member Pure: ^b -> (_ -> ^b)) b)

let inline pure' x : 'Pure'a =
    Pure.instance (Pure.Instances, Unchecked.defaultof<'Pure'a>) x

////////////////////////////////////////////////////////////////////////

module Map =
    type Instances = Instances with
        static member inline Map(m: 'a option) = fun f -> Option.map f m
        static member inline Map(m: 'a list)   = fun f -> List.map f m
        static member inline Map(m: 'a seq)    = fun f -> Seq.map f m
        static member inline Map(m: 'a array)  = fun f -> Array.map f m
        static member inline Map(m: 'a Async)  = fun f -> async.Bind (m, fun x -> async.Return (f x))
        static member inline Map(m: 'a Task)   = fun f -> m.ContinueWith (fun (t: 'a Task) -> f t.Result)

    let inline instance (a: ^a, b: ^b) =
        ((^a or ^b) : (static member Map: ^b -> (_ -> _)) b)

let inline map (f: 'a -> 'b) (m: 'Map'a) : 'Map'b =
    Map.instance (Map.Instances, m) f

////////////////////////////////////////////////////////////////////////

[<EntryPoint>]
let main argv =
    let xs = pure' 10 : int list

    let ys = map (fun x -> x + 2)
          << map (fun x -> x * 2)
          <| xs

    let zs = map (fun x -> x + 2) (map (fun x -> x * 2) xs)

    printfn "%A" xs
    printfn "%A" ys
    printfn "%A" zs

    0
