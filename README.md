# elm-review-optimize

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to optimize Elm code. This is meant to be used in addition to [`elm-review-simplify`](https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/).

## Provided rules

- [`Optimize`](https://package.elm-lang.org/packages/miniBill/elm-review-optimize/1.0.0/Optimize).

## Configuration

```elm
module ReviewConfig exposing (config)

import Optimize
import Simplify -- from elm-review-simplify
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Simplify.rule
    , Optimize.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template miniBill/elm-review-optimize/example
```
