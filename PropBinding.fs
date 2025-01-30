namespace FabulousX

open System.Runtime.CompilerServices
open Fabulous
open FabulousX

type PropBindingRequest<'prop> = delegate of unit -> Prop<'prop>

[<AutoOpen>]
module PropBindingBuilders =
    type Context with

        static member inline Binding(prop: Prop<'prop>) = PropBindingRequest(fun () -> prop)

        static member inline PropBinding(prop: Prop<'prop>) = PropBindingRequest(fun () -> prop)

type PropBindingExtensions =
    [<Extension>]
    static member inline Bind
        (
            _: ComponentBuilder<'parentMsg, 'marker>,
            [<InlineIfLambda>] fn: PropBindingRequest<'prop>,
            [<InlineIfLambda>] continuation: 'prop -> ComponentBodyBuilder<'msg, 'marker>
        ) =
        ComponentBodyBuilder<'msg, 'marker> (fun envContext treeContext context bindings ->
            let key = int bindings
            let isFirstTime = (context.TryGetValue key).IsValueNone
            let prop = fn.Invoke()

            let currentValue = Prop.getValue prop

            context.SetValueInternal(key, currentValue)

            if isFirstTime then
                let propContext = Prop.getContext prop

                context.LinkDisposable(
                    $"prop_binding_{key}",
                    fun () -> propContext.RenderNeeded.Subscribe(fun () -> context.NeedsRender())
                )
                |> ignore

            (continuation currentValue)
                .Invoke(envContext, treeContext, context, bindings + 1<binding>))
