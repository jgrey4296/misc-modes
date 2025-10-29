;;; hexyl-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;; An... extension of the built-in emacs hexl mode,
;; for use with hexyl instead.
;;
;;

;;  -n, --length <N>          Only read N bytes from the input. The N argument can also
;;                            include a unit with a decimal prefix (kB, MB, ..) or binary
;;                            prefix (kiB, MiB, ..), or can be specified using a hex number.
;;                            The short option '-l' can be used as an alias.
;;                            Examples: --length=64, --length=4KiB, --length=0xff

;;  -c, --bytes <N>           An alias for -n/--length

;;  -s, --skip <N>            Skip the first N bytes of the input. The N argument can also
;;                            include a unit (see `--length` for details)
;;                            A negative value is valid and will seek from the end of the
;;                            file.

;;      --block-size <SIZE>   Sets the size of the `block` unit to SIZE (default is 512).
;;                            Examples: --block-size=1024, --block-size=4kB

;;  -v, --no-squeezing        Displays all input data. Otherwise any number of groups of
;;                            output lines which would be identical to the preceding group
;;                            of lines, are replaced with a line comprised of a single
;;                            asterisk.

;;      --color <WHEN>        When to use colors. The auto-mode only displays colors if the
;;                            output goes to an interactive terminal [default: always]
;;                            [possible values: always, auto, never]

;;      --border <STYLE>      Whether to draw a border with Unicode characters, ASCII
;;                            characters, or none at all [default: unicode] [possible
;;                            values: unicode, ascii, none]

;;  -p, --plain               Display output with --no-characters, --no-position,
;;                            --border=none, and --color=never.

;;  -C, --no-characters       Whether to display the character panel on the right.

;;  -P, --no-position         Whether to display the position panel on the left.

;;  -o, --display-offset <N>  Add N bytes to the displayed file position. The N argument can
;;                            also include a unit (see `--length` for details)
;;                            A negative value is valid and calculates an offset relative to
;;                            the end of the file.

;;      --panels <N>          Sets the number of hex data panels to be displayed.
;;                            `--panels=auto` will display the maximum number of hex data
;;                            panels based on the current terminal width. By default, hexyl
;;                            will show two panels, unless the terminal is not wide enough
;;                            for that.

;;  -g, --group-size <N>      Number of bytes/octets that should be grouped together.
;;                            Possible group sizes are 1, 2, 4, 8. The default is 1.
;;                            '--groupsize can be used as an alias (xxd-compatibility).

;;  -b, --base <B>            Sets the base used for the bytes. The possible options are
;;                            binary, octal, decimal, and hexadecimal. The default base is
;;                            hexadecimal.

;;      --terminal-width <N>  Sets the number of terminal columns to be displayed.
;;                            Since the terminal width may not be an evenly divisible by the
;;                            width per hex data column, this will use the greatest number
;;                            of hex data panels that can fit in the requested width but
;;                            still leave some space to the right.
;;                            Cannot be used with other width-setting options.

;;  -h, --help                Print help information

;;  -V, --version             Print version information

(defvar hexyl-args '()
  "Arguments passed to the hexyl call"
  )
(defvar hexyl-block-size 256 "The amount of bytes to display")
(defvar hexyl-block-offset 0 "Used to offset the active block")
(defvar hexyl-executable "hexyl"
  "The hexyl executable location, or its name if its in the path"
  )
(defvar hexyl-null-char "*")
(defvar hexyl-unprintable-char ".")
(defvar hexyl-lock-movement t)
(defvar hexyl-base 'hexademical
  "The Base representation for data, one of [binary, octal, decimal, hexademial]"
  )

;00000000: 0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789ABCDEF
(defvar-local hexyl-mode-map
  (make-sparse-keymap))

;; Fontlock:
;; List of '(regex (groupnum "face")+)
(rx-let ()
  (defconst hexyl-mode-font-lock-keywords
    "Highlighting for -mode"
    )
  )


(define-derived-mode hexyl-mode fundamental-mode
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map hexyl-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list hexyl-mode-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'hexyl-mode-syntactic-face-function)
  (set (make-local-variable 'indent-line-function) '(lambda (&optional indent)))
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table -mode-syntax-table)
  ;;
  (setq major-mode 'hexyl-mode)
  (setq mode-name "Hexyl")
  (run-mode-hooks)
  ;; (outline-minor-mode)
  ;; (yas-minor-mode)

  )

(defun +run-hexyl-on-file (&optional filename)
  (interactive)
  ;; Run hexyl
  ;; add to buffer
  ;; display buffer in hexyl-mode
  )

;; todo ascii follow
(defun hexyl-mode-next-block ()

  )
(defun hexyl-mode-prev-block ()

  )

(provide 'hexyl-mode)
;;; hexyl-mode.el ends here
