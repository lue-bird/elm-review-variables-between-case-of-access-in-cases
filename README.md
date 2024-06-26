# [elm-review-variables-between-case-of-access-in-cases](https://package.elm-lang.org/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/1.0.2/)

Provides the [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule [`VariablesBetweenCaseOf.AccessInCases.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/1.0.2/VariablesBetweenCaseOf-AccessInCases/#forbid)
which reports when a variable between `case .. of` is used in any of the cases.

### reported

```elm
case arguments of
    [] ->
        "no arguments"

    _ :: _ ->
        arguments |> String.join ", "
```

### not reported

```elm
case arguments of
    [] ->
        "no arguments"

    firstArgument :: secondArgumentUp ->
        (firstArgument :: secondArgumentUp) |> String.join ", "
```

## Why

When you match a variable to a more specific case
and therefore have more information at hand,
you should use it!

There are some things that can easily go wrong otherwise:

### referring to the wrong variable

```elm
descriptionToString description =
    case description of
        SequenceDescription _ _ ->
            let
                expand description_ =
                    case description_ of
                        SequenceDescription early late ->
                            expand early ++ expand late

                        NonSequenceDescription _ ->
                            nonSequenceDescriptionToString description
            in
            expand description |> String.join ", then "

        NonSequenceDescription _ ->
            nonSequenceDescriptionToString description
```

Everything is good, right?

```elm
case description_ of
    ...
    NonSequenceDescription _ ->
        nonSequenceDescriptionToString description
```

Oops, it's the wrong description variable...

Another example that I've actually written:
```elm
listRemoveAt index list =
    ...
    case list of
        [] ->
            []

        head :: tail ->
            case index of
                0 ->
                    tail

                indexAtLeast1 ->
                    head :: (list |> listRemoveAt (indexAtLeast1 - 1))
```
Look at the last line where we call `listRemoveAt` recursively with the original list instead of `tail`.

### ignoring information you should use

```elm
descriptionAndErrorToString description error =
    case ( description, error ) of
        ( EndDescription _, EndError customErrorMessage ) ->
            "expected " ++ descriptionToString description ++ " but " ++ customErrorMessage

        ( EndDescription _, _ ) ->
            "custom with unexpected error kind: " ++ descriptionToString description ++ " but " ++ errorToString error

        ( CustomDescription _, CustomError customErrorMessage ) ->
            "expected " ++ descriptionToString description ++ " but " ++ customErrorMessage

        ( CustomDescription _, _ ) ->
            "custom with unexpected error kind: " ++ descriptionToString description
```

Found the error? You didn't add the error String in the case `( CustomDescription _, _ ) ->`.
If you had just always used the error from the specific case, ignoring the error would have been more obvious

### not using the extra guarantees

I see code _like_ below a lot.
```elm
highestScoreUi scores =
    case scores of
        [] ->
            textUi "no scores, yet"
        
        scoresHead :: _ ->
            case List.maximum scores of
                -- This can never happen
                Nothing ->
                    -- so we'll just use the head or whatever
                    scoreUi scoresHead
                
                Just highestFound ->
                    scoreUi highestFound
```
Don't look away, I know you've written code like that.
Let's just blame it on thinking patterns from other languages where you can't safely represent your data
and elm for providing so many `-> Maybe` operations.

Unwrap late and use the extra information of your specific case:
```elm
highestScoreUi =
    case scores of
        [] ->
            textUi "no scores, yet"
        
        scoresHead :: scoresTail ->
            scoreUi (List.Nonempty.minimum ( scoresHead, scoresTail ))
```
If you think this is not possible in your case, write me anytime @lue on slack!

For example: "But how would I do a case on `Dict`-emptiness?
  - → There are non-empty dict implementations with `fromDict`
  - → there are more convenient representations like [`KeysSet`](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/) that you can actually case on

Maybe this is not your grind, but I think precise data modeling in elm is fun.
Give it a go even if you think it's not practical and see what happens!

## try it out

```bash
elm-review --template lue-bird/elm-review-variables-between-case-of-access-in-cases/example
```

## configure locally

```elm
module ReviewConfig exposing (config)

import VariablesBetweenCaseOf.AccessInCases
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ VariablesBetweenCaseOf.AccessInCases.forbid
    ]
```
