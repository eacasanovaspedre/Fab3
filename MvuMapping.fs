namespace FabulousX

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open Fabulous
open FabulousX

[<Struct>]
type MvuMapping<'model, 'mappedModel> =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public SourceContext: ComponentContext

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public SourceKey: int

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    val public Map: 'model -> 'mappedModel

    new(modelValueX: ModelValueX<'model>, map: 'model -> 'mappedModel) =
        { SourceContext = modelValueX.Context
          SourceKey = modelValueX.Key
          Map = map }

    member this.Current =
        this
            .SourceContext
            .TryGetValue<'model>(
                this.SourceKey
            )
            .Value
        |> this.Map

type MvuMappingRequest<'model, 'mappedModel> = delegate of unit -> MvuMapping<'model, 'mappedModel>

[<AutoOpen>]
module MvuMappingBuilders =
    type Context with
        [<Obsolete("Use Prop and Binding")>]
        static member inline MapMvu(modelValue: ModelValueX<'model>, map: 'model -> 'mappedModel) =
            MvuMappingRequest(fun () -> MvuMapping(modelValue, map))

type MvuMappingExtensions =
    [<Extension>]
    static member inline Bind
        (
            _: ComponentBuilder<'parentMsg, 'marker>,
            [<InlineIfLambda>] fn: MvuMappingRequest<'model, 'mappedModel>,
            [<InlineIfLambda>] continuation: 'mappedModel -> ComponentBodyBuilder<'msg, 'marker>
        ) =
        ComponentBodyBuilder<'msg, 'marker> (fun envContext treeContext context bindings ->
            let key = int bindings

            let mvuMapping =
                match context.TryGetValue<MvuMapping<'model, 'mappedModel>>(key) with
                | ValueSome mvuMapping ->
                    mvuMapping
                | ValueNone ->
                    let mvuMapping = fn.Invoke()
                    context.SetValue(key, mvuMapping)
                    
                    context.LinkDisposable(
                        $"mvu_mapping_{key}",
                        fun () -> mvuMapping.SourceContext.RenderNeeded.Subscribe (fun () -> //it would be great to have the key that triggered the render here, to avoid re-rendering on any change of the parent.
                            context.NeedsRender())) |> ignore
                    
                    mvuMapping

            (continuation mvuMapping.Current)
                .Invoke(envContext, treeContext, context, bindings + 1<binding>))
