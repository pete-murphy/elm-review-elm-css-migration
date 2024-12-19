# elm-review-elm-css-migration

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to assist in migrating off of [`elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/).

## Provided rules

- [`OnlyInAttributeList`](https://package.elm-lang.org/packages/pete-murphy/elm-review-elm-css-migration/1.0.2/OnlyInAttributeList/) - Reports instances of `css` outside of list of attributes.

## Configuration

```elm
module ReviewConfig exposing (config)

importOnlyInAttributeList
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ OnlyInAttributeList.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template pete-murphy/elm-review-elm-css-migration/example
```
