open System.Threading.Tasks

////////////////////////////////////////////////////////////////////////

module Pure =
    type Instances = Instances with
        static member inline Pure(_: 'a option) = fun (x: 'a) -> Some x
        static member inline Pure(_: 'a list)   = fun (x: 'a) -> [x]
        static member inline Pure(_: 'a seq)    = fun (x: 'a) -> Seq.singleton x
        static member inline Pure(_: 'a array)  = fun (x: 'a) -> [|x|]
        static member inline Pure(_: 'a Async)  = fun (x: 'a) -> async.Return (x : 'a)
        static member inline Pure(_: 'a Task)   = fun (x: 'a) -> Task.FromResult (x : 'a)

    let inline instance (a: ^a, b: ^b) =
        ((^a or ^b) : (static member Pure: ^b -> (_ -> ^b)) b)

let inline pure' x : 'Pure'a =
    Pure.instance (Pure.Instances, Unchecked.defaultof<'Pure'a>) x

////////////////////////////////////////////////////////////////////////

module Map =
    type Instances = Instances with
        static member inline Map(m: 'a option) = fun (f: 'a -> 'b) -> Option.map f m
        static member inline Map(m: 'a list)   = fun (f: 'a -> 'b) -> List.map f m
        static member inline Map(m: 'a seq)    = fun (f: 'a -> 'b) -> Seq.map f m
        static member inline Map(m: 'a array)  = fun (f: 'a -> 'b) -> Array.map f m
        static member inline Map(m: 'a Async)  = fun (f: 'a -> 'b) -> async.Bind (m, fun x -> async.Return (f x))
        static member inline Map(m: 'a Task)   = fun (f: 'a -> 'b) -> m.ContinueWith (fun (t: 'a Task) -> f t.Result)

    let inline instance (a: ^a, b: ^b) =
        ((^a or ^b) : (static member Map: ^b -> (_ -> _)) b)

let inline map (f: 'a -> 'b) (m: 'Functor'a) : 'Functor'b =
    Map.instance (Map.Instances, m) f

let inline (<!>) (f: 'a -> 'b) (m: 'Functor'a) : 'Functor'b =
    map f m

////////////////////////////////////////////////////////////////////////

module Bind =
    type Instances = Instances with
        static member inline Bind(b: 'a option) = fun (f: 'a -> 'b option) -> Option.bind f b
        static member inline Bind(b: 'a list)   = fun (f: 'a -> 'b list)   -> List.collect f b
        static member inline Bind(b: 'a seq)    = fun (f: 'a -> 'b seq)    -> Seq.collect f b
        static member inline Bind(b: 'a array)  = fun (f: 'a -> 'b array)  -> Array.collect f b
        static member inline Bind(b: 'a Async)  = fun (f: 'a -> 'b Async)  -> async.Bind (b, f) : 'b Async
        static member inline Bind(b: 'a Task)   = fun (f: 'a -> 'b Task)   -> b.ContinueWith(fun (t: 'a Task) -> f t.Result).Unwrap()

    let inline instance (a: ^a, b: ^b) =
        ((^a or ^b) : (static member Bind: ^b -> (_ -> _)) b)

let inline (>>=) (b: 'Monad'a) (f: 'a -> 'Monad'b) : 'Monad'b =
    Bind.instance (Bind.Instances, b) f

let inline ap (f: 'Monad'a_'b) (m: 'Monad'a) : 'Monad'b =
    f >>= fun f' -> m >>= fun x -> pure' (f' x)

////////////////////////////////////////////////////////////////////////

module Apply =
    type Instances = Instances with
        static member inline Apply(a: 'a option) = fun (f: ('a -> 'b) option) -> ap f a
        static member inline Apply(a: 'a list)   = fun (f: ('a -> 'b) list)   -> ap f a
        static member inline Apply(a: 'a seq)    = fun (f: ('a -> 'b) seq)    -> ap f a
        static member inline Apply(a: 'a array)  = fun (f: ('a -> 'b) array)  -> ap f a
        static member inline Apply(a: 'a Async)  = fun (f: ('a -> 'b) Async)  -> ap f a
        static member inline Apply(a: 'a Task)   = fun (f: ('a -> 'b) Task)   -> ap f a

    let inline instance (a: ^a, b: ^b) =
        ((^a or ^b) : (static member Apply: ^b -> (_ -> _)) b)

let inline (<*>) (f: 'Applicative'a_'b) (a: 'Applicative'a) : 'Applicative'b =
    Apply.instance (Apply.Instances, a) f

////////////////////////////////////////////////////////////////////////

type MonadBuilder () =
    member inline __.Bind(m, f)    = m >>= f
    member inline __.Return(x)     = pure' x
    member inline __.Let(m, f)     = f m
    member inline __.ReturnFrom(x) = x

let monad = MonadBuilder ()

////////////////////////////////////////////////////////////////////////

type Fold =
    abstract init : 'x
    abstract step : 'x -> 'a -> 'x
    abstract fine : 'x -> 'b

type Fold<'a, 'b, 'c> = {
    init : 'a
    step : 'a -> 'b -> 'b
    fine : 'b -> 'c
}

////////////////////////////////////////////////////////////////////////

[<EntryPoint>]
let main argv =
    let xs = pure' 10 : int list

    let ys = map (fun x -> x + 2)
          << map (fun x -> x * 2)
          <| xs

    let zs = xs >>= fun x -> [x+1; x+2; x+3]
                >>= fun x -> [x*10; x*100; x*1000]

    let ws = monad {
               let! x = xs
               let! y = [x+1; x+2; x+3]
               return! [y*10; y*100; y*1000]
             }

    printfn "%A" xs
    printfn "%A" ys
    printfn "%A" zs
    printfn "%A" ws

    0
