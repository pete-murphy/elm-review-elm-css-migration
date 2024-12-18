# elm-review-elm-css-migration

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`ElmCssMigration`](https://package.elm-lang.org/packages/pete-murphy/elm-review-elm-css-migration/1.0.0/ElmCssMigration) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import ElmCssMigration
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ ElmCssMigration.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template pete-murphy/elm-review-elm-css-migration/example
```
