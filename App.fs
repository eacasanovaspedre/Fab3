namespace Fab3

open FabulousX
open Fabulous
open Fabulous.Maui

open type Fabulous.Maui.View

module Counter =
    type Model = { Count: int }

    type Msg = | Clicked

    let init () = { Count = 0 }, Cmd.none

    let update notifyParent msg model =
        match msg with
        | Clicked ->
            let newCount = model.Count + 1

            if newCount % 5 = 0 then
                { model with Count = 0 }, Cmd.ofEffect (fun _ -> notifyParent ())
            else
                { model with Count = newCount }, Cmd.none

    let program dispatchToParent =
        Program.statefulWithCmd init (update dispatchToParent)

    let view dispatchToParent (prop: Prop<_>)  =
        let program = Program.statefulWithCmd init (update dispatchToParent)
        (Component "Counter" {
            let! model = Context.Mvu program
            let! timesFive = Context.PropBinding prop

            let text =
                if model.Count = 0 then
                    $"Click me"
                else
                    $"Clicked {model.Count} times"

            VStack(spacing = 25.) {
                Label $"Value in child %d{timesFive}"
                Button(text, Clicked).centerHorizontal ()
            }
        })

module App =
    
    type AppModel = { NumberOfTimesFiveWasReached: int array }

    type Msg = | MultipleOfFiveReached of int

    let init _ = { NumberOfTimesFiveWasReached = [| 0; 0; 0; 0 |]; }

    let update msg { NumberOfTimesFiveWasReached = x } =
        match msg with
        | MultipleOfFiveReached index -> { NumberOfTimesFiveWasReached = x |> Array.mapi (fun i v -> if i = index then v + 1 else v) }

    let program () : _ * Program<_, _, _> =
        let ev = Event<int>()

        ev,
        Program.stateful init update
        |> Program.withSubscription (fun _ ->
            [ [ "whatever" ], (fun dispatch -> ev.Publish.Subscribe(fun index -> dispatch (MultipleOfFiveReached index))) ])

    let program1 () : Program<_, _, _> =
        Program.stateful init update

    let view () =
        let e, p = program ()

        (Component "Fab 3" {

            let! modelValue = Context.Mvu p
            //let! childProp = Context.Prop(modelValue, _.NumberOfTimesFiveWasReached)
            let! childProp = Context.Prop modelValue.NumberOfTimesFiveWasReached

            let { NumberOfTimesFiveWasReached = numberOfTimesFiveWasReached } =
                modelValue

            Application() {
                Window(
                    ContentPage(
                        ScrollView(
                            VStack(spacing = 25.) {
                                Label $"Value in parent: %A{numberOfTimesFiveWasReached}"
                                for i in 0..numberOfTimesFiveWasReached.Length - 1 do
                                    let p = Prop.map (Array.item i) childProp
                                    Counter.view (fun () -> e.Trigger i) p //this now works
                            }
                        )
                    )
                )
            }
        })