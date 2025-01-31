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

    let view name index dispatchToParent (prop: Prop<_>) =
        let program = Program.statefulWithCmd init (update dispatchToParent)

        (Component $"Counter-{name}" {
            printfn $"EACP Component {name}-{index}"
            let! model = Context.Mvu program
            let! timesFive = Context.PropBinding prop

            let text =
                if model.Count = 0 then
                    $"Click me"
                else
                    $"Clicked {model.Count} times"

            VStack(spacing = 25.) {
                Label $"Value in child %s{name} %d{timesFive}"
                Button(text, Clicked).centerHorizontal ()
            }
        })

module App =

    type AppModel =
        { NumberOfTimesFiveWasReached: (string * int) array }

    type Msg = MultipleOfFiveReached of int

    let init _ =
        { NumberOfTimesFiveWasReached =
            [| ("Toyota", 0)
               ("Audi", 0)
               ("Hyundai", 0)
               ("Kia", 0) |] }

    let update msg { NumberOfTimesFiveWasReached = x } =
        match msg with
        | MultipleOfFiveReached index ->
            { NumberOfTimesFiveWasReached =
                x
                |> Array.mapi (fun i (n, v) -> if i = index then (n, v + 1) else (n, v))
                |> Array.sortBy (snd >> (~-)) }

    let program () : _ * Program<_, _, _> =
        let ev = Event<int>()

        ev,
        Program.stateful init update
        |> Program.withSubscription (fun _ ->
            [ [ "whatever" ],
              (fun dispatch ->
                  ev.Publish.Subscribe (fun index ->
                      printfn $"EACP Event subscription triggered {index}"
                      dispatch (MultipleOfFiveReached index))) ])

    let program1 () : Program<_, _, _> = Program.stateful init update

    let view () =
        let e, p = program ()

        Application() {
            Window(
                ContentPage(
                    ScrollView(
                        Component "Fab 3" {
                            let! _ = Context.Prop 5
                            let! modelValue = Context.Mvu p
                            let! childProp = Context.Prop modelValue.NumberOfTimesFiveWasReached

                            let { NumberOfTimesFiveWasReached = numberOfTimesFiveWasReached } = modelValue

                            VStack(spacing = 25.) {
                                yield Label $"Value in parent: %A{numberOfTimesFiveWasReached}"

                                for i in 0 .. numberOfTimesFiveWasReached.Length - 1 do
                                    let p = Prop.map (Array.item i >> snd) childProp
                                    let name = Array.item i numberOfTimesFiveWasReached |> fst

                                    yield
                                        Counter.view
                                            name
                                            i
                                            (fun () ->
                                                printfn $"EACP Triggering event {i}"
                                                e.Trigger i)
                                            p //this now works
                            }
                        }
                    )
                )
            )
        }
