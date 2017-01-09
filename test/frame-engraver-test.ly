\require "frame-engraver:.."

\relative c'' {
  \override Stem #'transparent = ##t
  \override Beam #'transparent = ##t
  \once \override Frame #'extender-length = #8
  \once \override Frame #'left-extra-padding = #-1
  \once \override Frame #'right-extra-padding = #0
  \frameStart dis'8[ e f \frameEnd ges] s2
  \once \override Staff.BarLine #'X-extent = #'(0 . 1) % ugh.
  \once \override Frame #'extender-length = #12
  \frameStart d,,8[ e f \frameEnd g] s2
  s4
  \once \override Frame #'extender-length = #3.5
  \frameStart f'8[ aes g \frameEnd bes] s4
}

\layout {
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
  \context {
    \Voice
    \consists \frameEngraver
  }
}


