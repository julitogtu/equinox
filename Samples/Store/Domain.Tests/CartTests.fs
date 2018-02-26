﻿module Samples.Store.Domain.Tests.CartTests

open Domain
open Domain.Cart
open Domain.Cart.Events
open Domain.Cart.Folds
open Domain.Cart.Commands
open Swensen.Unquote
open TypeShape.Empty
open System

let mkAddQty skuId qty          = ItemAdded { empty<ItemAddInfo> with skuId = skuId; quantity = qty }
let mkAdd skuId                 = mkAddQty skuId 1
let mkRemove skuId              = ItemRemoved { empty<ItemRemoveInfo> with skuId = skuId }
let mkChangeWaived skuId value  = ItemWaiveReturnsChanged { empty<ItemWaiveReturnsInfo> with skuId = skuId; waived = value }

/// As a basic sanity check, verify the basic properties we'd expect per command if we were to run it on an empty stream
let verifyCanProcessInInitialState cmd (originState: State) =
    let events = interpret cmd originState
    match cmd with
    | Compact
    | AddItem _ ->
        test <@ (not << List.isEmpty) events @>
    | ChangeItemQuantity _
    | ChangeWaiveReturns _
    | RemoveItem _ ->
        test <@ List.isEmpty events @>

/// Put the aggregate into the state where the command should trigger an event; verify correct events are yielded
let verifyCorrectEventGenerationWhenAppropriate command (originState: State) =
    let initialEvents = command |> function
        | Compact ->                                [ (* Command is not designed to be idempotent *) ]
        | AddItem _ ->                              []
        | ChangeItemQuantity (_, skuId, quantity) ->[ mkAddQty skuId (quantity+1)]
        | ChangeWaiveReturns (_, skuId, value) ->   [ mkAdd skuId; mkChangeWaived skuId (not value) ]
        | RemoveItem (_, skuId) ->                  [ mkAdd skuId ]
    let state = fold originState initialEvents
    let events = interpret command state
    let state' = fold state events

    let find skuId                                  = state'.items |> List.find (fun x -> x.skuId = skuId)
    match command, events with
    | Compact,                                      [ Compacted e ] ->
        let expectedState = State.ofCompacted e
        test <@ expectedState = state' @>
    | AddItem (_, csku, quantity),                  [ ItemAdded e ] ->
        test <@ { ItemAddInfo.context = e.context; skuId = csku; quantity = quantity } = e
                && quantity = (find csku).quantity @>
    | ChangeItemQuantity (_, csku, value),          [ ItemQuantityChanged e] ->
        test <@ { ItemQuantityChangeInfo.context = e.context; skuId = csku; quantity = value } = e
                && value = (find csku).quantity @>
    | ChangeWaiveReturns (_, csku, value),          [ ItemWaiveReturnsChanged e] ->
        test <@ { ItemWaiveReturnsInfo.context = e.context; skuId = csku; waived = value } = e
               && value = (find csku).returnsWaived @>
    | RemoveItem (_, csku),                         [ ItemRemoved e ] ->
        test <@ { ItemRemoveInfo.context = e.context; skuId = csku } = e
                && not (state'.items |> List.exists (fun x -> x.skuId = csku)) @>
    | c,e -> failwithf "Invalid result - Command %A yielded Events %A in State %A" c e state

/// Processing should allow for any given Command to be retried at will
let verifyIdempotency (cmd: Command) (originState: State) =
    // Put the aggregate into the state where the command should not trigger an event
    let establish: Event list = cmd |> function
        | Compact _ ->                              let skuId = SkuId (Guid.NewGuid()) in [ mkAdd skuId; mkRemove skuId]
        | AddItem (_, skuId, qty) ->                [ mkAddQty skuId qty]
        | ChangeItemQuantity (_, skuId, qty) ->     [ mkAddQty skuId qty]
        | ChangeWaiveReturns (_, skuId, value) ->   [ mkAdd skuId; mkChangeWaived skuId value ]
        | RemoveItem _ ->                           []
    let state = fold originState establish
    let events = interpret cmd state
    match cmd, not (List.isEmpty events) with
    | Compact, hasEvents ->
        // Command should be unconditional
        test <@ hasEvents @>
    | _, hasEvents ->
        // Assert we decided nothing needs to happen
        test <@ not hasEvents @>

[<DomainProperty(MaxTest = 1000)>]
let ``interpret yields correct events, idempotently`` (cmd: Command) (originState: State) =
    verifyCanProcessInInitialState cmd originState
    verifyCorrectEventGenerationWhenAppropriate cmd originState
    verifyIdempotency cmd originState