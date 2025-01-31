namespace FabulousX

open System.Runtime.CompilerServices
open Fabulous

[<Struct>]
type Prop<'prop> =
    | ValueProp of Context: ComponentContext * Key: int
    | MappedProp of Context: ComponentContext * GetValue: (unit -> 'prop)

module internal Prop =

    let inline getValue prop =
        match prop with
        | ValueProp (context, key) -> context.TryGetValue<'prop>(key).Value
        | MappedProp (_, getValue) -> getValue ()

    let inline getContext prop =
        match prop with
        | ValueProp (context, _)
        | MappedProp (context, _) -> context

    let inline map f prop =
        MappedProp(getContext prop, (fun () -> getValue prop |> f))

type PropRequest<'prop> = 'prop
//| OfMapping of Prop<'srcProp> * ('srcProp -> 'prop)

[<AutoOpen>]
module PropBuilders =
    type Context with

        static member inline Prop(value: 'value) = value

type PropExtensions =
    [<Extension>]
    static member inline Bind
        (
            _: ComponentBuilder<'parentMsg, 'marker>,
            request: PropRequest<'prop>,
            [<InlineIfLambda>] continuation: Prop<'prop> -> ComponentBodyBuilder<'msg, 'marker>
        ) =
        ComponentBodyBuilder<'msg, 'marker> (fun envContext treeContext context bindings ->
            let key = int bindings
            printfn $"EACP Prop %A{request} is first time: {context.TryGetValue(key).IsNone}"
            context.SetValueInternal(key, request)

            (continuation (ValueProp(context, key)))
                .Invoke(envContext, treeContext, context, bindings + 1<binding>)
        )
