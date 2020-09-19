;;;; Lisp Code to Cycle Fonts
(defun cycle-font (num)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined set of fonts.
If NUM is 1, cycle forward.
If NUM is -1, cycle backward.
Warning: tested on Windows Vista only."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontList fontToUse currentState nextState )
    (setq fontList (list
                    "Courier New-11" "DejaVu Sans Mono-11" "Sans-11"
                    "Verdana-11" "B&H LucidaTypeWriter-11" "Arial-11"
                    "Monospace-11" "Andale Mono-11" "Bitstream Terminal-14"
                    "DEC Terminal-11" "Droid Sans Mono-11" "Efont Fixed-11"
                    "ETL Fixed-11" "FreeMono-11" "Liberation Mono-11"
                    "Luxi Mono-11" "Misc Fixed-11" "Misc Fixed Wide-10"
                    "Nimbus Mono L-11" "Schumacher Clean-9"
                    "Sony Fixed-12"
                    ))
    ;; fixed-width "Courier New" "Unifont"  "FixedsysTTF" "Miriam Fixed" "Lucida Console" "Lucida Sans Typewriter"
    ;; variable-width "Code2000"
    (setq currentState (if (get 'cycle-font 'state) (get 'cycle-font 'state) 0))
    (setq nextState (% (+ currentState (length fontList) num) (length fontList)))

    (setq fontToUse (nth nextState fontList))
    (set-frame-parameter nil 'font fontToUse)
    (redraw-frame (selected-frame))
    (message "Current font is: %s" fontToUse )

    (put 'cycle-font 'state nextState)
    )
  )

(defun cycle-font-var (num)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined set of fonts.
If NUM is 1, cycle forward.
If NUM is -1, cycle backward.
Warning: tested on Windows Vista only."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontList fontToUse currentState nextState )
    (setq fontList (list
"Sans Serif-11" "Adobe Helvetica-11"
"Adobe New Century Schoolbook-11"
"Adobe Times-11" "Adobe Utopia-11"
"Arial Black-11" "B&H Lucida-11" "B&H LucidaBright-11" "Bitstream Charter-11"
"Cantarell-11" "Century Schoolbook L-11" "Charter-11" "Clean-9"
"Comic Sans MS-11"
"Computer Modern-11"
"Computer Modern Bright-11"
"Computer Modern Concrete-11"
"Dejavu Sans Condensed-11"
"Droid Sans-11"
"Efont Biwidth-11" "Estrangelo Antioch-11"
"Estrangelo Edessa-11" "Estrangelo Midyat-11" "Estrangelo Nisibin-11"
"Estrangelo Nisibin Outline-11" "Estrangelo Quenneshrin-11"
"Estrangelo Talada-11" "Estrangelo TurAbdin-11"
"FreeEuro-11"
"FreeSans-11"
"FreeSerif-11" "Georgia-11" "Goha-Tibeb Zemen-11"
"Latin Modern Mono Prop-11"
"Latin Modern Sans-11"
"Latin Modern Sans Demi Cond-11"
"Liberation Mono-11" "Liberation Sans-11"
"Liberation Sans Narrow-11"
"Luxi Sans-11"
"Nimbus Sans L-11" "Nimbus Sans L Condensed-11"
"Philokalia-11"
"Serto Batnan-11" "Serto Jerusalem-11" "Serto Jerusalem Outline-11"
"Serto Kharput-11" "Serto Malankara-11" "Serto Mardin-11" "Serto Urhoy-11"
"Trebuchet MS-11" "URW Bookman L-11" "URW Chancery L-11" "URW Gothic L-11"
"URW Palladio L-11"
"Utopia-11"
"Verdana-11"
                   ))
    ;; fixed-width "Courier New" "Unifont"  "FixedsysTTF" "Miriam Fixed" "Lucida Console" "Lucida Sans Typewriter"
    ;; variable-width "Code2000"
    (setq currentState (if (get 'cycle-font 'state) (get 'cycle-font 'state) 0))
    (setq nextState (% (+ currentState (length fontList) num) (length fontList)))

    (setq fontToUse (nth nextState fontList))
    (set-frame-parameter nil 'font fontToUse)
    (redraw-frame (selected-frame))
    (message "Current font is: %s" fontToUse )

    (put 'cycle-font 'state nextState)
    )
  )

(defun cycle-font-forward ()
  "Switch to the next font, in the current frame.
See `cycle-font'."
  (interactive)
  (cycle-font 1)
  )

(defun cycle-font-var-forward ()
  "Switch to the next font, in the current frame.
See `cycle-font-var'."
  (interactive)
  (cycle-font-var 1)
  )

(defun cycle-font-backward ()
  "Switch to the previous font, in the current frame.
See `cycle-font'."
  (interactive)
  (cycle-font -1)
  )

(defun cycle-font-var-backward ()
  "Switch to the previous font, in the current frame.
See `cycle-font-var'."
  (interactive)
  (cycle-font-var -1)
  )

(global-set-key (kbd "<f5> F") 'cycle-font-forward)
(global-set-key (kbd "<f5> B") 'cycle-font-backward)
(global-set-key (kbd "<f5> f") 'cycle-font-var-forward)
(global-set-key (kbd "<f5> b") 'cycle-font-var-backward)
