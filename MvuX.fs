namespace FabulousX

open System.ComponentModel
open System.Runtime.CompilerServices
open Fabulous

[<Struct>]
type ModelValueX<'T> =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public Context: ComponentContext

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public Key: int

    val public Current: 'T

    new(ctx, key, value) =
        { Context = ctx
          Key = key
          Current = value }

type MvuRequestX<'arg, 'model, 'msg> = delegate of unit -> struct (Program<'arg, 'model, 'msg> * 'arg)

[<AutoOpen>]
module MvuBuildersX =
    type Context with
        static member inline MvuX(program: Program<unit, 'model, 'msg>) =
            MvuRequestX<unit, 'model, 'msg>(fun () -> program, ())

        static member inline MvuX(program: Program<'arg, 'model, 'msg>, arg: 'arg) =
            MvuRequestX<'arg, 'model, 'msg>(fun () -> program, arg)

type MvuExtensionsX =
    [<Extension>]
    static member Bind
        (
            _: ComponentBuilder<'parentMsg, 'marker>,
            fn: MvuRequestX<'arg, 'model, 'msg>,
            continuation: ModelValueX<'model> -> ComponentBodyBuilder<'msg, 'marker>
        ) =
        ComponentBodyBuilder<'msg, 'marker> (fun envContext treeContext context bindings ->
            let key = int bindings

            let struct (treeContext, state) =
                match context.TryGetValue(key) with
                | ValueSome state -> treeContext, state
                | ValueNone ->
                    let struct (program, arg) = fn.Invoke()

                    let runner =
                        context.LinkDisposable(
                            "runner",
                            fun () ->
                                let getModel () =
                                    match context.TryGetValue<'model>(key) with
                                    | ValueNone ->
                                        failwith (
                                            "Model not found in ComponentContext "
                                            + context.Id.ToString()
                                        )
                                    | ValueSome model -> model

                                let setModel v = context.SetValue(key, v)

                                new Runner<'arg, 'model, 'msg>(getModel, setModel, program)
                        )

                    // Redirect messages to runner
                    let treeContext = { treeContext with Dispatch = unbox >> runner.Dispatch }

                    runner.Start(arg)

                    let state = context.TryGetValue(key).Value

                    treeContext, state

            let modelValueX = ModelValueX(context, key, state)

            (continuation modelValueX)
                .Invoke(envContext, treeContext, context, bindings + 1<binding>))
