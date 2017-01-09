%\version "2.17.23"

% Uncomment the following line to show the skylines.
%#(ly:set-option 'debug-skylines)

%#(set-global-staff-size 10)

% Note: In order to prevent "bleed-over" from one session to another, it is
% recommended that .ly files using the following functions be processed
% individually (rather than in a batch).

%%%%%%%%%%%%%%%%%%%%%%%%%%% CREATE NEW EVENT CLASSES %%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOTE: The following definitions work as of version 2.17.22.  If you are using
% an earlier version, please see
% https://codereview.appspot.com/10965043/diff/8001/input/regression/scheme-text-spanner.ly
% to make necessary changes.

#(define-event-class 'frame-event 'span-event)

#(define-event-class 'frame-extender-event 'span-event)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MUSIC DESCRIPTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define frame-types
   '(
     (FrameEvent
      . ((description . "Signals where a frame starts and stops.")
         (types . (general-music frame-event span-event event))
         ))
     ))

#(define frame-extender-types
  '(
     (FrameExtenderEvent
      . ((description . "Signals where a frame extender line stops.")
         (types . (general-music frame-extender-event span-event event))
         ))
     ))

#(for-each
  (lambda (x)
    (set! x
          (map
            (lambda (x)
              (set-object-property! (car x)
                                    'music-description
                                    (cdr (assq 'description (cdr x))))
              (let ((lst (cdr x)))
                (set! lst (assoc-set! lst 'name (car x)))
                (set! lst (assq-remove! lst 'description))
                (hashq-set! music-name-to-property-table (car x) lst)
                (cons (car x) lst)))
            x))
    (set! music-descriptions
          (append x music-descriptions)))
  (list frame-types frame-extender-types))

#(set! music-descriptions
       (sort music-descriptions alist<?))

%%%%%%%%%%%%%%%%%%%%%%%%% ADD NEW GROB INTERFACES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(ly:add-interface
  'frame-interface
  "A box for frame notation."
  '(frame-elements))

#(ly:add-interface
  'frame-extender-interface
  "An extender line (with arrow) for frame notation."
  '(frame))

#(ly:add-interface
  'frame-bracket-interface
  "A bracket with text (possibly to indicate duration) for frame notation."
  '(bracket-elements columns edge-height
    extender no-bracket side-support-elements))

#(ly:add-interface
  'frame-stub-interface
  "A stub for horizontal spacing of frames."
  '(no-arrowhead elements frame))

%%%%%%%%%%%%%%%%%%%%%%%%% CREATE NEW GROB PROPERTIES %%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (ly:error (_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  symbol)

#(map
  (lambda (x)
    (apply define-grob-property x))

  `(
    (extender-Y-offset ,number?
      "Vertical displacement of extender line from center staff line")
    (no-arrowhead ,boolean?
      "If #t, do not draw an arrowhead on a frame extender line.  Default is #f.")
    (no-bracket ,boolean?
      "If #t, do not draw bracket in frame notation.  Default is #f.")
    (repeat-barlines ,boolean?
      "If #t, use repeat barlines in frame notation.  Otherwise, use boxes.")
  ))

%%%%%%%%%%%%%%%%%%%%%%%% WIDTH/HEIGHT FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Thanks to Mike Solomon for explanations and code.

% The following functions calculate dimensions for those grobs which have
% stencils: Frame, FrameExtender, and FrameBracket.  Grobs with stencils
% should not use the 'axis-group-interface for calculating height and width.
% Relevant grobs have been stored in arrays other than 'elements, which
% is the grob-array used by the axis-group-interface for its calculations.

#(define (dim-hack grob ax)
  (let* ((frame-elts (ly:grob-object grob 'frame-elements))
         (common (ly:grob-common-refpoint-of-array grob frame-elts ax))
         (rel (ly:relative-group-extent frame-elts common ax))
         (off (ly:grob-relative-coordinate grob common ax)))
    ;(format #t "rel-~a: ~a  off: ~a~%" ax rel off)
    (coord-translate rel (- off))))

#(define (height-hack grob)
  (dim-hack grob Y))

#(define (width-hack grob)
  (dim-hack grob X))

#(define (bracket-width grob)
  (let* ((bracket-elts (ly:grob-object grob 'bracket-elements))
         (common (ly:grob-common-refpoint-of-array grob bracket-elts X))
         (rel (ly:relative-group-extent bracket-elts common X))
         (off (ly:grob-relative-coordinate grob common X)))
    (coord-translate rel (- off))))

% The FrameStub is a sort of spacer which is added to the left and right
% of Frame grobs.

#(define (frame-stub::width grob)
"Select the appropriate callback for FrameStub.X-extent depending on whether
frame notation uses repeat barlines or boxes."
  (let* ((frame (ly:grob-object grob 'frame))
         (repeat-barlines? (ly:grob-property frame 'repeat-barlines)))
    (if (not repeat-barlines?)
        (frame-stub::width-with-box grob)
        (frame-stub::width-with-repeat-barlines grob))))

#(define (frame-stub::width-with-box grob)
  (let* ((elts (ly:grob-object grob 'elements))
         (staff-space (ly:staff-symbol-staff-space grob))
         (frame (ly:grob-object grob 'frame))
         (box-padding (* (ly:grob-property frame 'padding) staff-space))
         (frame-th (* (ly:grob-property frame 'thickness) staff-space))
         (direction (ly:grob-property grob 'direction))
         (extra-space (* 0.5 staff-space))
         (axis-group-width (ly:axis-group-interface::width grob)))

  (if (eq? direction LEFT)
      (cons (- (car axis-group-width) frame-th box-padding extra-space)
            (car axis-group-width))
      (cons (cdr axis-group-width)
            (+ (cdr axis-group-width) box-padding frame-th extra-space)))))

% The following is selected if you are using repeat-bars-with-extender
% notation.

#(define (frame-stub::width-with-repeat-barlines grob)
  (let* ((elts (ly:grob-object grob 'elements))
         (axis-group-width (ly:axis-group-interface::width grob))
         (staff-space (ly:staff-symbol-staff-space grob))
         ;; The stencil extent of the repeat barline must be calculated here.
         ;; While the frame is accessible, attempting to get relevant imformation
         ;; about its extents has disastrous results.
         (line-thickness (ly:staff-symbol-line-thickness grob))
         (thick-bar (* 6.0 line-thickness))
         (thin-bar (* 1.9 line-thickness))
         (kern (* 3.0 line-thickness))
         (dot-radius (/ staff-space 5.0))
         (bar-extent (+ thick-bar kern thin-bar kern (* dot-radius 2.0)))
         (direction (ly:grob-property grob 'direction))
         (frame (ly:grob-object grob 'frame))
         (box-padding (* (ly:grob-property frame 'padding) staff-space)))
  (if (eq? direction LEFT)
      (cons (- (car axis-group-width) bar-extent box-padding)
            (car axis-group-width))
      (cons (cdr axis-group-width)
            (+ (cdr axis-group-width) bar-extent box-padding)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRINT FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (frame::print grob)
"Select the appropriate print function for frame notation.  Stencil options are
a box or repeat barlines encompassing the contents."
  (if (not (ly:grob-property grob 'repeat-barlines))
      (frame::print-with-box grob)
      (frame::print-with-repeat-barlines grob)))

#(define (frame::print-with-box grob)
"Draw a box around a group of notes.  For use in frame notation."
  (let* ((staff-space (ly:staff-symbol-staff-space grob)) ; for scaling
         (box-padding (* (ly:grob-property grob 'padding) staff-space))
         ; cannot use axis-group-interface, as there are no elements
         (height (height-hack grob))
         (width (width-hack grob))
         (th (* (ly:grob-property grob 'thickness) staff-space))
         (stencil (ly:make-stencil '() width height))
         (stencil (box-stencil stencil th box-padding)))
    stencil))

% adapted from scm\bar-line.scm

#(define (make-thin-barline grob extent)
  "Draw a simple barline.  The width is constant."
  (let* ((line-thickness (layout-line-thickness grob))
         (thickness (* 1.9 line-thickness)))
    (ly:round-filled-box (cons 0.0 thickness)
                         extent
                         0.0)))

#(define (make-thick-barline grob extent)
  "Draw a thick barline.  The width is constant."
  (let* ((line-thickness (layout-line-thickness grob))
         (thickness (* 6.0 line-thickness)))
    (ly:round-filled-box (cons 0.0 thickness)
                         extent
                         0.0)))

#(define (make-colon-barline grob extent)
  "Draw repeat dots.  Scaled to match staff size."
  (let* ((staff-space (ly:staff-symbol-staff-space grob))
         (line-thickness (ly:staff-symbol-line-thickness grob))
         (dot-radius (/ staff-space 5))
         (dot (make-circle-stencil dot-radius 0.0 #t)))
     (ly:stencil-add
       (ly:stencil-combine-at-edge point-stencil
         Y UP dot (* 0.25 staff-space))
       (ly:stencil-combine-at-edge point-stencil
         Y DOWN dot (* 0.25 staff-space)))))

#(define (frame::print-with-repeat-barlines grob)
"Draw repeat barlines encompassing a group of notes.  For use in frame notation."
  (let* ((staff-space (ly:staff-symbol-staff-space grob)) ; for scaling
         (box-padding (* (ly:grob-property grob 'padding) staff-space))
         (staff-radius (* (ly:staff-symbol-staff-radius grob) staff-space))
         (height (cons (- staff-radius) staff-radius))
         (width (width-hack grob))
         ;; value is scaled to reflect constant line thickness
         (line-thickness (layout-line-thickness grob))
         ;; kern, thick-bar, and thin-bar have widths independent of staff size
         (kern (* 3.0 line-thickness))
         (thick-bar (make-thick-barline grob height))
         (thin-bar (make-thin-barline grob height))
         (colon-bar (make-colon-barline grob height))
         (left-bar
           (ly:stencil-combine-at-edge
             (ly:stencil-combine-at-edge thick-bar X RIGHT thin-bar kern)
             X RIGHT colon-bar kern))
         (left-bar-ext (ly:stencil-extent left-bar X))
         (right-bar
           (ly:stencil-combine-at-edge
             (ly:stencil-combine-at-edge colon-bar X RIGHT thin-bar kern)
             X RIGHT thick-bar kern))
         (right-bar-ext (ly:stencil-extent right-bar X))
         (stencil
           (ly:stencil-add
             (ly:stencil-translate-axis
               left-bar
               (- (car width) (interval-length left-bar-ext) box-padding)
               X)
           (ly:stencil-translate-axis
             right-bar
             (+ (cdr width) box-padding)
             X))))
    stencil))

#(define (ly:frame-extender::print grob)
"Draw an extender line with optional arrow for use in frame notation."
  (let* ((refp (ly:grob-system grob))
         (staff-space (ly:staff-symbol-staff-space grob)) ; for scaling
         (line-th (layout-line-thickness grob)) ; scaled for uniform thickness
         ;; NOTE: The frame grob is only available to the portion of a broken
         ;; extender which is on the same line.  The following lookup will
         ;; return '() for siblings.  It would be nice to have the option
         ;; to center the extender vertically on the frame, and have the
         ;; siblings at the same height.  How can this be done automatically
         ;; without the frame being "remembered"?
         (frame (ly:grob-object grob 'frame))
         (frame-ext
           (if (ly:grob? frame)
               (ly:grob-extent frame refp X)
               (cons 0.0 0.0)))
         (frame-th
           (if (ly:grob? frame)
               (* (ly:grob-property frame 'thickness) staff-space)
               0.0))
         (left-bound (ly:spanner-bound grob LEFT))
         (left-bound-coord (ly:grob-relative-coordinate left-bound refp X))
         (left-bound-ext (ly:grob-extent left-bound left-bound X))
         ;; Calculate an offset so that extender segment begins within the
         ;; right side of the frame, or clears prefatory material if a
         ;; broken piece.
         (start
           (if (ly:grob? frame)
               (- (cdr frame-ext) left-bound-coord (* 0.5 frame-th))
               (+ (cdr left-bound-ext) (* 1.0 staff-space))))
         (right-bound (ly:spanner-bound grob RIGHT))
         (right-bound-coord (ly:grob-relative-coordinate right-bound refp X))
         (right-bound-ext (ly:grob-extent right-bound right-bound X))
         (right-offset
           (if (interval-empty? right-bound-ext)
               0.0
               (interval-length right-bound-ext)))
         ;; Ensure that there is some space after end of extender line.
         (right-offset (+ right-offset (* 0.5 staff-space)))
         (end (- right-bound-coord left-bound-coord right-offset))
         ;; By default, the line will be drawn at the center line of the staff.
         (extender-dy (* (ly:grob-property grob 'extender-Y-offset) staff-space))
         (extender-th (* (ly:grob-property grob 'thickness) staff-space))
         (half-extender-th (/ extender-th 2.0))
         ;; Line is drawn as filled box because there is no rounding
         ;; of corners.  Rounded corners make alignment with frame
         ;; problematic.  For this reason, the public function
         ;; `arrow-stencil-maker' from `stencil.scm' has not been used.
         (extender
           (markup
            #:override '(filled . #t)
             #:path
               line-th
               ;; Do not draw arrowhead if extender line isn't longer
               ;; than the arrowhead, or per user request
               (if (or
                     (<= (- end start) (+ (* 3.5 extender-th) frame-th))
                     (ly:grob-property grob 'no-arrowhead))

                   `((moveto ,start ,(+ extender-dy half-extender-th))
                     (lineto ,end
                             ,(+ extender-dy half-extender-th))
                     (lineto ,end
                             ,(- extender-dy half-extender-th))
                     (lineto ,start ,(- extender-dy half-extender-th))
                     (closepath))

                  `(; line with arrowhead
                    (moveto ,start ,(+ extender-dy half-extender-th))
                    (lineto ,(- end (* 3 extender-th) half-extender-th)
                            ,(+ extender-dy half-extender-th))
                    (lineto ,(- end (* 3 extender-th) half-extender-th)
                            ,(- extender-dy half-extender-th))
                    (lineto ,start ,(- extender-dy half-extender-th))
                    (closepath)
                    ; the arrowhead
                    (moveto ,end ,extender-dy)
                    (curveto
                     ,end
                     ,extender-dy
                     ,(- end (* 1.5 extender-th) (/ half-extender-th 2))
                     ,extender-dy
                     ,(- end (* 3 extender-th) half-extender-th)
                     ,(+ extender-dy (* 1.5 extender-th)))
                    (lineto ,(- end (* 3 extender-th) half-extender-th)
                            ,(- extender-dy (* 1.5 extender-th)))
                    (curveto
                      ,(- end (* 3 extender-th) half-extender-th)
                      ,(- extender-dy (* 1.5 extender-th))
                      ,(- end (* 1.5 extender-th) (/ half-extender-th 2))
                      ,extender-dy
                      ,end
                      ,extender-dy)
                    (closepath)))))
         (extender (grob-interpret-markup grob extender)))
      extender))

#(define (ly:frame-bracket::print grob)
"Draw a bracket for use in frame notation."
  (let* ((orig (ly:grob-original grob))
         (staff-space (ly:staff-symbol-staff-space grob)) ; for scaling
         (line-th (layout-line-thickness grob))
         (siblings (ly:spanner-broken-into orig))
         (width (bracket-width grob))
         ;; We need the blot-diameter so that there is no gap between the
         ;; horizontal line and the vertical edges of the bracket
         (layout (ly:grob-layout grob))
         (blot (ly:output-def-lookup layout 'blot-diameter))
         (text (ly:grob-property grob 'text))
         ;; if no text is specified, create a dummy stencil.
         (text (if (null? text)
                   (markup #:null)
                   (markup #:scale (cons staff-space staff-space) text)))
         (text (grob-interpret-markup grob text))
         ;; center the text on its own extents and those of the bracket
         (text (ly:stencil-aligned-to text X CENTER))
         (text (ly:stencil-translate-axis text (car width) X))
         (text (ly:stencil-translate-axis text (/ (interval-length width) 2) X))
         (stil (make-line-stencil line-th
                                  (car width) 0
                                  (cdr width) 0))
         (dir (ly:grob-property grob 'direction))
         ;; create the vertical lines for bracket edges
         (edge-height (* 1.0 staff-space))
         (protrusion (make-line-stencil line-th
                                        0 0
                                        0 (* edge-height (- dir)))))

  ;; Add edges and text depending on whether we have a broken bracket.

    ;; Left edge:
    (if (or
          (and (>= (length siblings) 2)
               (eq? grob (first siblings)))
          (< (length siblings) 2))
        (set! stil
              (ly:stencil-combine-at-edge stil X LEFT protrusion (- blot))))
    ;; Right edge:
    (if (or
          (and (>= (length siblings) 2)
               (eq? grob (last siblings)))
          (< (length siblings) 2))
        (set! stil
              (ly:stencil-combine-at-edge stil X RIGHT protrusion (- blot))))
    ;; Text is only added to the first piece of a broken bracket.  Possibly
    ;; the text should be added in parentheses to the pieces (as in a measure
    ;; count).  0.5 is a hardcoded padding value which controls the separation
    ;; between the text and the bracket.
    (if (or
          (and (>= (length siblings) 2) (eq? grob (first siblings)))
          (< (length siblings) 2))
        (set! stil
              (ly:stencil-combine-at-edge stil Y dir text (* 0.5 staff-space))))
    stil))

%%%%%%%%%%%%%%%%%%%%%%%%% PURE HEIGHT ESTIMATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A pure height function is necessary so that the FrameBracket is taken into
% account in page-breaking decisions.  Without this function, the spacing engine
% will fit too many systems on the page.  The result is that an error will be
% returned and the music will be compressed (regardless of collisions).

#(define (frame-bracket-interface::pure-height grob start end)
  (let* ((text (ly:grob-property grob 'text (markup #:null)))
         (staff-space (ly:staff-symbol-staff-space grob))
         (padding (* (ly:grob-property grob 'padding) staff-space))
         (text-padding (* 0.5 staff-space)) ; distance between bracket and text
         ;; height of bracket "wings"
         (edge-height (* 1.0 staff-space))
         (text-stil (grob-interpret-markup grob (markup text)))
         (text-stil-ext (ly:stencil-extent text-stil Y)))
    (cons (- (+ edge-height padding))
          (+ text-padding (interval-length text-stil-ext)))))

%%%%%%%%%%%%%%%%%%%%%%%%% ADD NEW GROB DEFINITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (add-grob-definition grob-name grob-entry)
   (let* ((meta-entry   (assoc-get 'meta grob-entry))
          (class        (assoc-get 'class meta-entry))
          (ifaces-entry (assoc-get 'interfaces meta-entry)))
     (set-object-property! grob-name 'translation-type? ly:grob-properties?)
     (set-object-property! grob-name 'is-grob? #t)
     (set! ifaces-entry (append (case class
                                  ((Item) '(item-interface))
                                  ((Spanner) '(spanner-interface))
                                  ((Paper_column) '((item-interface
                                                     paper-column-interface)))
                                  ((System) '((system-interface
                                               spanner-interface)))
                                  (else '(unknown-interface)))
                                ifaces-entry))
     (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
     (set! ifaces-entry (cons 'grob-interface ifaces-entry))
     (set! meta-entry (assoc-set! meta-entry 'name grob-name))
     (set! meta-entry (assoc-set! meta-entry 'interfaces
                                  ifaces-entry))
     (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
     (set! all-grob-descriptions
           (cons (cons grob-name grob-entry)
                 all-grob-descriptions))))

#(add-grob-definition
  'Frame
  `((padding . 0.8)
    (stencil . ,frame::print)
    (thickness . 0.3)
    (repeat-barlines . #f)
    (meta . ((class . Spanner)
             (interfaces . (frame-interface
                            line-interface))))))

#(add-grob-definition
  'FrameExtender
  `((extender-Y-offset . 0.0)
    (no-arrowhead . #f)
    (stencil . ,ly:frame-extender::print)
    (thickness . 0.3)
    (meta . ((class . Spanner)
             (interfaces . (frame-extender-interface
                            line-interface))))))

#(add-grob-definition
  'FrameBracket
  `((direction . ,UP)
    (no-bracket . #f)
    (outside-staff-padding . 1.0)
    (outside-staff-priority . 0)
    (padding . 1.0)
    (side-axis . ,Y)
    (stencil . ,ly:frame-bracket::print)
    (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
    (Y-extent . ,(grob::unpure-Y-extent-from-stencil frame-bracket-interface::pure-height))
    (Y-offset . ,ly:side-position-interface::y-aligned-side)
    (meta . ((class . Spanner)
             (interfaces . (frame-bracket-interface
                            line-interface
                            side-position-interface))))))

#(add-grob-definition
  'FrameStub
  `((X-extent . ,frame-stub::width)
    (Y-extent . ,axis-group-interface::height)
    (meta . ((class . Item)
             (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                  (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
             (interfaces . (frame-stub-interface))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FRAME ENGRAVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (add-bound-item spanner item)
"Set left and right bounds of a spanner"
  (if (null? (ly:spanner-bound spanner LEFT))
      (ly:spanner-set-bound! spanner LEFT item)
      (ly:spanner-set-bound! spanner RIGHT item)))

frameEngraver =
#(lambda (context)
  (let ((frame '()) ; the box enclosing the items to be repeated
        (extender '()) ; a horizontal line with arrow
        (frame-start '()) ; the event which signals the beginning of a frame
        (frame-stop '()) ; signals the end of a frame
        (extender-stop '()) ; the event which signals end of extender line
        (bracket '()) ; a bracket with text (i.e., timing)
        ;; In order to handle the situation when a FrameBracket begins in the
        ;; same timestep as another ends (i.e., when the user calls
        ;; \frameExtenderEnd and \frameStart together), some shuffling of
        ;; grobs between variables is necessary.  We copy the spanner held in
        ;; 'bracket' to 'finished-bracket' once it has been begun.  We will
        ;; complete it there.  This frees 'bracket' to contain the beginnings
        ;; of another FrameBracket grob.
        (finished-bracket '())
        ;; A spacer for the left and right sides of the Frame grob.  This is
        ;; necessary to prevent collisions between the Frame and grobs which
        ;; precede and follow it, such as barlines.
        (frame-stub '()))

    (make-engraver
      (listeners
        ((frame-event engraver event)
         (if (= START (ly:event-property event 'span-direction))
             (set! frame-start event)
             (set! frame-stop event)))
        ((frame-extender-event engraver event)
         ;; The extender is begun automatically when a frame ends, so we listen
         ;; only for the extender's termination event.
         (if (= STOP (ly:event-property event 'span-direction))
             (set! extender-stop event))))

      (acknowledgers
        ((note-column-interface engraver grob source-engraver)
         (if (ly:spanner? frame)
             (begin
               (ly:pointer-group-interface::add-grob frame 'frame-elements grob)
               ;; The box is attached to a note column on left and right sides.
               (add-bound-item frame grob)))
         (if (ly:spanner? extender)
             ;; The extender line is attached to a note column on the left.
             ;; A NonMusicalPaperColumn will be the right bound.
             (if (null? (ly:spanner-bound extender LEFT))
                 (ly:spanner-set-bound! extender LEFT grob)))
         (if (ly:spanner? bracket)
             (begin
               (ly:pointer-group-interface::add-grob bracket 'columns grob)
               ;; Bracket is attached to a NonMusicalPaperColumn on either side.
               (add-bound-item bracket
                               (ly:context-property context 'currentCommandColumn))))
         (if (ly:spanner? finished-bracket)
             (begin
               (ly:pointer-group-interface::add-grob finished-bracket 'columns grob)))
         (if (ly:item? frame-stub)
             (ly:pointer-group-interface::add-grob frame-stub 'elements grob)))
        ((script-interface engraver grob source-engraver)
         (if (and (ly:spanner? frame)
                  (not (ly:grob-property frame 'repeat-barlines)))
             (ly:pointer-group-interface::add-grob frame 'frame-elements grob))
         (if (and (ly:item? frame-stub)
                  (not (ly:grob-property frame 'repeat-barlines)))
             (ly:pointer-group-interface::add-grob frame-stub 'elements grob)))
        ;((text-script-interface engraver grob source-engraver)
         ;(if (ly:spanner? frame)
             ;(ly:pointer-group-interface::add-grob frame 'frame-elements grob)))
        ((inline-accidental-interface engraver grob source-engraver)
         ;; Frame will be sized to accommodate leading accidental.
         (if (ly:spanner? frame)
             (ly:pointer-group-interface::add-grob frame 'frame-elements grob))
         (if (ly:item? frame-stub)
             (ly:pointer-group-interface::add-grob frame-stub 'elements grob))))

      ((process-music trans)
       ;; When the user requests a frame (with \frameStart), we begin a
       ;; Frame and a FrameBracket.  A FrameStub is created for the left
       ;; side of the Frame.
       (if (ly:stream-event? frame-start)
           (begin
             (set! frame (ly:engraver-make-grob trans 'Frame frame-start))
             (set! bracket
                   (ly:engraver-make-grob trans 'FrameBracket frame-start))
             (ly:pointer-group-interface::add-grob
               bracket 'bracket-elements frame)
             (ly:pointer-group-interface::add-grob
               bracket 'side-support-elements frame)
             (set! frame-stub
                   (ly:engraver-make-grob trans 'FrameStub frame-start))
             (ly:grob-set-object! frame-stub 'frame frame)
             (ly:grob-set-property! frame-stub 'direction LEFT)
             (set! frame-start '())))
       ;; User has requested completion of Frame grob.
       (if (ly:stream-event? frame-stop)
           ;; Error: user has written \frameEnd without \frameStart.
           ;; Nothing will be drawn.
           (if (null? frame)
               (ly:programming-error "No start to frame.  Nothing drawn.")
               ;; no error: finish Frame, begin FrameExtender, and add references
               ;; to the completed Frame in FrameExtender and FrameBracket.
               ;; Create FrameStub for right side of Frame.
               (begin
                 (ly:engraver-announce-end-grob trans frame frame-stop)
                 (set! extender
                       (ly:engraver-make-grob
                         trans 'FrameExtender frame-stop))
                 (ly:grob-set-object! extender 'frame frame)
                 (set! frame-stub
                       (ly:engraver-make-grob trans 'FrameStub frame-stop))
                 (ly:grob-set-object! frame-stub 'frame frame)
                 (ly:grob-set-property! frame-stub 'direction RIGHT)
                 ;; Store FrameBracket-in-progress in another variable
                 ;; so that new FrameBracket might be begun at same timestep.
                 (set! finished-bracket bracket)
                 (set! bracket '()))))
       ;; When we hear an extender-event, we finish both FrameExtender and
       ;; FrameBracket
       (if (ly:stream-event? extender-stop)
           ;; If there is an extender to end...
           (if (ly:spanner? extender)
               (let ((col (ly:context-property context 'currentCommandColumn)))
                 (ly:spanner-set-bound! extender RIGHT col)
                 (ly:engraver-announce-end-grob trans extender extender-stop)
                 (ly:spanner-set-bound! finished-bracket RIGHT col)
                 (ly:grob-set-object! finished-bracket 'extender extender)
                 (ly:pointer-group-interface::add-grob
                   finished-bracket 'bracket-elements extender)
                 (ly:pointer-group-interface::add-grob
                   finished-bracket 'side-support-elements extender)
                 (ly:engraver-announce-end-grob
                   trans finished-bracket extender-stop)
                 (if (ly:grob-property finished-bracket 'no-bracket)
                     (ly:grob-suicide! finished-bracket))
                 (set! extender '())
                 (set! finished-bracket '())
                 (set! extender-stop '()))
               ;; There is a request to end a non-existent extender...
               (set! extender-stop '()))))

      ((stop-translation-timestep trans)
       (set! frame-stub '())
       ;; Frame is complete.
       (if (ly:stream-event? frame-stop)
           (begin
             (set! frame '())
             (set! frame-stop '()))))

       ((finalize trans)
        ;; If there is an incomplete extender at the end of the context, the
        ;; variable 'extender' won't be empty.  Finish the extender and bracket
        ;; with a warning.
        (if (ly:spanner? extender)
            (let ((col (ly:context-property context 'currentCommandColumn)))
              (ly:warning "You didn't finish final extender.  Completing...")
              (ly:spanner-set-bound! extender RIGHT col)
              (ly:spanner-set-bound! finished-bracket RIGHT col)
              (ly:grob-set-object! finished-bracket 'extender extender)
              (ly:pointer-group-interface::add-grob
                finished-bracket 'bracket-elements extender)
              (ly:pointer-group-interface::add-grob
                finished-bracket 'side-support-elements extender)
              (ly:engraver-announce-end-grob
                trans finished-bracket extender-stop)
              (if (ly:grob-property finished-bracket 'no-bracket)
                  (ly:grob-suicide! finished-bracket))
              (set! extender '())
              (set! extender-stop '())
              (set! finished-bracket '())))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EVENT FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

frameStart =
#(make-span-event 'FrameEvent START)

frameEnd =
#(make-span-event 'FrameEvent STOP)

% There is no `frameExtenderStart' because extender is begun with \frameEnd
frameExtenderEnd =
#(make-span-event 'FrameExtenderEvent STOP)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
