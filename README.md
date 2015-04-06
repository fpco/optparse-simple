optparse-simple
=====

Simple interface to optparse-applicative

## Usage

Typical usage with no commands:

``` haskell
do (opts,()) <-
     simpleOptions "ver"
                   "header"
                   "desc"
                   (flag () () (long "some-flag"))
                   empty
   doThings opts
```

Typical usage with commands:

``` haskell
do (opts,runCmd) <-
     simpleOptions "ver"
                   "header"
                   "desc"
                   (pure ()) $
     do addCommand "delete"
                   "Delete the thing"
                   (const deleteTheThing)
                   (pure ())
        addCommand "create"
                   "Create a thing"
                   createAThing
                   (strOption (long "hello"))
   runCmd
```
