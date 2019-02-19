namespace Equinox

/// Maintains a rolling folded State while Accumulating Events decided upon as part of a decision flow
type Accumulator<'event, 'state>(fold : 'state -> 'event seq -> 'state, originState : 'state) =
    let accumulated = ResizeArray<'event>()

    /// The Events that have thus far been pended via the `decide` functions `Execute`/`Decide`d during the course of this flow
    member __.Accumulated : 'event list =
        accumulated |> List.ofSeq

    /// The current folded State, based on the Stream's `originState` + any events that have been Accumulated during the the decision flow
    member __.State : 'state =
        accumulated |> fold originState

    /// Invoke a decision function, gathering the events (if any) that it decides are necessary into the `Accumulated` sequence
    member __.Execute(decide : 'state -> 'event list) : unit =
        decide __.State |> accumulated.AddRange
    /// Invoke an Async decision function, gathering the events (if any) that it decides are necessary into the `Accumulated` sequence
    member __.ExecuteAsync(decide : 'state -> Async<'event list>) : Async<unit> = async {
        let! events = decide __.State
        accumulated.AddRange events }
    /// As per `Execute`, invoke a decision function, while also propagating a result yielded as the fst of an (result, events) pair
    member __.Decide(decide : 'state -> 'result * 'event list) : 'result =
        let result, newEvents = decide __.State
        accumulated.AddRange newEvents
        result
    /// As per `ExecuteAsync`, invoke a decision function, while also propagating a result yielded as the fst of an (result, events) pair
    member __.DecideAsync(decide : 'state -> Async<'result * 'event list>) : Async<'result> = async {
        let! result, newEvents = decide __.State
        accumulated.AddRange newEvents
        return result }

#if false

/// Transaction builder PoC

type [<NoComparison; NoEquality>] TransactionBuilder<'State, 'Event, 'Result> =
    internal TB of (ResizeArray<'Event> -> 'State -> 'Result * 'State)

type TransactionBuilder<'State, 'Event>(apply : 'State -> 'Event -> 'State) =
    member __.Zero() = TB (fun _ s -> (), s)
    member __.Delay (f : unit -> TransactionBuilder<'State, 'Event, 'Result>) = TB (fun es s -> let (TB f') = f () in f' es s)
    member __.Return x = TB (fun _ s -> x,s)
    member __.ReturnFrom (TB _ as tb) = tb
    member __.Bind(TB f : TransactionBuilder<'State, 'Event, 'T>, g : 'T -> TransactionBuilder<'State, 'Event, 'S>) =
        TB (fun es s -> let t,s' = f es s in let (TB g') = g t in g' es s')

    member __.Combine(TB f : TransactionBuilder<'State, 'Event, unit>, TB g : TransactionBuilder<'State, 'Event, 'R>) =
        TB (fun es s ->
            let (),s' = f es s
            g es s')

    member __.For(xs : seq<'T>, body : 'T -> TransactionBuilder<'State, 'Event, unit>) : TransactionBuilder<'State, 'Event, unit> =
        TB (fun es s ->
            let mutable s = s
            for x in xs do let (TB f) = body x in let (),s' = f es s in s <- s'
            (), s)

    member __.Yield (e : 'Event) = TB (fun acc s -> acc.Add e; (), apply s e)
    member __.YieldFrom (es : seq<'Event>) = TB (fun acc s -> let es = Seq.toArray es in acc.AddRange es; (), Array.fold apply s es)

    member __.Run(TB f) = TB (fun s -> let es = ResizeArray() in let r, _ = f es s in (r, Seq.toList es))

[<AutoOpen>]
module Operators2 =
    let tb apply = TransactionBuilder<_,_> (apply)
    let getState = TB(fun _ s -> s,s)
    let setState s = TB(fun _ _ -> (), s)

    // alternatively can expose as .Run() method in the builder, however that would remove ability to compose transaction builders
    let toTransaction (TB f) = TB (fun s -> let es = ResizeArray() in let r, _ = f es s in (r, Seq.toList es))

// example
module Example =

    let addBuilder = tb (fun s i -> s + i)

    addBuilder {
        for i in 1 .. 10 do
            yield i
            let! s = getState
            printfn "State=%d" s

        // design choices: either make composable or Disable `Run` operation
        let! x = addBuilder { return 42 }

        return! getState
    } |> toTransaction |> run 0

#endif