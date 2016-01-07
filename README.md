# live-vdom-todomvc

Implementation of the todomvc with live-vdom

# Building

To build inside a sandbox you need [live-vdom](https://github.com/plow-technologies/live-vdom). Inside the live-vdom package there is a non-hackage-dependencies folder. You will need to add these to the sandbox too.
To create the sandbox, run `cabal sandbox init`

To add live-vdom and the other dependencies:
```
cabal sandbox add-source /path/to/live-vdom
cabal sandbox add-source /path/to/live-vdom/non-hackage-dependencies/*
```

You will also need (valentine)[https://github.com/plow-technologies/valentine].

```
cabal sandbox add-source /path/to/valentine
```


# Running

To build the project you need to configure, install, and build with ghcjs

```
cabal configure --ghcjs
cabal install --ghcjs
cabal build
```

Now you can open the index in a browser