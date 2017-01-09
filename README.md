# frame-engraver: Engrave boxes around notes in Lilypond

This package provides an engraver that draws boxes around groups of notes. The code herein was written by [David Nalesnik](https://github.com/davidnalesnik) and repackaged into a [lyp](https://github.com/noteflakes/lyp) package. For more context read the messages on lilypond-user:

- http://lilypond.1069038.n5.nabble.com/Aleatoric-modern-notation-tp18113p18117.html
- http://lilypond.1069038.n5.nabble.com/help-with-frameEngraver-bars-and-boxes-function-td199021.html

## Installation

```bash
$ lyp install frame-engraver
```

## Usage

```lilypond
\require "frame-engraver"

...
```

For more information see the included [example](test/frame-engraver-test.ly).

