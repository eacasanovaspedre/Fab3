namespace Fab3

open FabulousX
open Fabulous
open Fabulous.Maui

open type Fabulous.Maui.View

type AppModel = { NumberOfTimesFiveWasReached: int }

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

    let view dispatchToParent parentModelValue (*this now works*) =
        let program = Program.statefulWithCmd init (update dispatchToParent)

        (Component("Counter") {
            let! timesFive = Context.MapMvu (parentModelValue, fun { NumberOfTimesFiveWasReached = x } -> x)
            let! model = Context.Mvu program
            

            let text =
                if model.Count = 0 then
                    $"Click me"
                else
                    $"Clicked {model.Count} times"

            VStack(spacing = 25.) {
                Label($"Value in child {timesFive}")
                Button(text, Clicked).centerHorizontal ()
            }
        })

module App =

    type Msg = | MultipleOfFiveReached

    let init _ = { NumberOfTimesFiveWasReached = 0 }

    let update msg { NumberOfTimesFiveWasReached = x } =
        match msg with
        | MultipleOfFiveReached -> { NumberOfTimesFiveWasReached = x + 1 }

    let program () : _ * Program<_, _, _> =
        let ev = Event<unit>()

        ev,
        Program.stateful init update
        |> Program.withSubscription (fun _ ->
            [ [ "whatever" ], (fun dispatch -> ev.Publish.Subscribe(fun _ -> dispatch MultipleOfFiveReached)) ])

    let view () =
        let e, p = program ()

        (Component("Fab 3") {

            let! modelValue = Context.MvuX p
            let { NumberOfTimesFiveWasReached = numberOfTimesFiveWasReached } = modelValue.Current

            Application() {
                Window(
                    ContentPage(
                        ScrollView(
                            VStack(spacing = 25.) {
                                Label($"Value in parent: {numberOfTimesFiveWasReached}")
                                Counter.view (fun _ -> e.Trigger(())) modelValue //this now works
                            }
                        )
                    )
                )
            }
        })
