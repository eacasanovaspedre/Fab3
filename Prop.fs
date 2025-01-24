namespace FabulousX

open System.ComponentModel
open System.Runtime.CompilerServices
open Fabulous
open FabulousX

[<Struct>]
type Prop<'prop> =

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public SourceContext: ComponentContext
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public PropContext: ComponentContext
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public PropKey: int

    new(sourceContext, propContext, propKey) =
        { SourceContext = sourceContext; PropContext = propContext; PropKey = propKey }

    member inline internal this.Current =
        this.PropContext.TryGetValue<Lazy<'prop>>(this.PropKey).Value.Value

type PropRequest<'model, 'prop> = 
    delegate of unit ->
        {| SourceContext: ComponentContext
           Evaluate: unit -> 'prop |}

[<AutoOpen>]
module PropBuilders =
    type Context with

        static member inline Prop(modelValue: ModelValueX<'model>, map: 'model -> 'prop) =
            PropRequest (fun () ->
                {| SourceContext = modelValue.Context
                   Evaluate = fun () -> map modelValue.Current |})

type PropExtensions =
    [<Extension>]
    static member inline Bind
        (
            _: ComponentBuilder<'parentMsg, 'marker>,
            [<InlineIfLambda>] fn: PropRequest<'model, 'prop>,
            [<InlineIfLambda>] continuation: Prop<'prop> -> ComponentBodyBuilder<'msg, 'marker>
        ) =
        ComponentBodyBuilder<'msg, 'marker> (fun envContext treeContext context bindings ->
            let key = int bindings

            let requestResult = fn.Invoke()

            context.SetValueInternal(key, Lazy.Create requestResult.Evaluate)

            (continuation (Prop(requestResult.SourceContext, context, key)))
                .Invoke(envContext, treeContext, context, bindings + 1<binding>))
