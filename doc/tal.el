;;; TAL Mode
;;;
;;; (C) Neal Glew, July 1998, all rights reserved.
;;; NG 29 Jul 1998 - Initial version

;;; To use tal mode add the following to your .emacs file:
;;;   (setq auto-mode-alist (cons '("\\.tali?$" . tal-mode) auto-mode-alist))
;;;   (autoload 'tal-mode "tal" "Major mode for editing TAL files." t)
;;; To fontify in tal mode add either:
;;;    (setq tal-do-fontify t)
;;; or (add-hook 'tal-mode-hook 'tal-fontify)

;;; Hooks

(defvar tal-mode-hook nil
  "*List of functions to call when entering TAL mode.")

;;; Keymap

(defvar tal-mode-map nil
  "Keymap for TAL major mode.")
(if tal-mode-map nil (setq tal-mode-map (make-sparse-keymap)))

;;; Syntax Table

(defvar tal-mode-syntax-table nil
  "Syntax table for TAL major mode.")
(if tal-mode-syntax-table
    nil
  (setq tal-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?! "/" tal-mode-syntax-table)
  (modify-syntax-entry ?\; "<" tal-mode-syntax-table)
  (modify-syntax-entry ?\n ">" tal-mode-syntax-table)
  (modify-syntax-entry ?< "(>" tal-mode-syntax-table)
  (modify-syntax-entry ?> ")<" tal-mode-syntax-table)
  (modify-syntax-entry ?$ "w" tal-mode-syntax-table)
  (modify-syntax-entry ?_ "w" tal-mode-syntax-table)
  (modify-syntax-entry ?\? "w" tal-mode-syntax-table)
  (modify-syntax-entry ?% " " tal-mode-syntax-table)
  (modify-syntax-entry ?& " " tal-mode-syntax-table)
  (modify-syntax-entry ?\# " " tal-mode-syntax-table)
  (modify-syntax-entry ?\' " " tal-mode-syntax-table)
  (modify-syntax-entry ?* "." tal-mode-syntax-table)
  (modify-syntax-entry ?+ "." tal-mode-syntax-table)
  (modify-syntax-entry ?- " " tal-mode-syntax-table)
  (modify-syntax-entry ?/ " " tal-mode-syntax-table)
  (modify-syntax-entry ?= "." tal-mode-syntax-table)
  (modify-syntax-entry ?\\ " " tal-mode-syntax-table)
  (modify-syntax-entry ?\| "." tal-mode-syntax-table)
  (modify-syntax-entry ?~ " " tal-mode-syntax-table))

;;; Fontification

(defvar tal-do-fontify nil
  "*Turn on font lock in TAL mode.")

(defconst tal-font-lock-keywords
  (list
   ; comments
   '(";.*" . font-lock-comment-face)
   ; types
   '("<\\([^>!]\\|!.\\)*>\\|S([0-9]*)" . font-lock-type-face)
   ; strings
   '("\"[ !#-~]*\"" . font-lock-string-face)
   ; keywords
   '("\\<\\(A\\(LEN\\|SUB\|UPD\\|ll\\)\\|B\\(1\\|2\\|4\\|8\\|\\(A\\|V\\)\\(1\\|2\\|4\\|8\\)\\|EXN\\|TAG\\(I\\|VAR\\)\\)\\|CO\\(DE\\|ERCE\\)\\|DATA\\|E\\(ND\\|xist\\)\\|FALLTHRU\\|INCLUDE\\|LABELTYPE\\|MALLOC\\|R\\(\\|L\\)\\|S\\|T\\(\\|4\\|AL_\\(IMPORT\\|EXPORT\\)\\|YPE\\|app\\|s\\)\\|UNPACK\\|VAL\\|a\\(len\\|rray\\|sub\|upd\\)\\|b\\(exn\\|tag\\(i\\|var\\)\\)\\|co\\(de\|erce\\)\\|data\\|e\\(nd\\|xn\\(\\|name\\)\\)\\|f\\(allthru\\|n\\)\\|junk\\|labeltype\\|malloc\\|of\\|pack\\|r\\(e\\(al\\|c\\)\\|oll\\(\\|sum\\)\\)\\|s\\(e\\|lot\\|ptr\\|um\\)\\|t\\(a\\(l_\\(import\\|export\\)\\|pp\\)\\|ype\\)\\|un\\(pack\\|roll\\)\\|v\\(al\\|ector\\)\\|_\\(begin_TAL\\|end_TAL\\)\\)\\>" . font-lock-keyword-face)
   ; labels
   '("[a-zA-Z_$?][a-zA-Z_@$?0-9]*[ \\t\\f]*:" . font-lock-variable-name-face)
   )
  "Keywords to highlight in TAL mode.")

(defun tal-fontify ()
  (if window-system
      (progn
	(make-local-variable 'font-lock-defaults)
	(setq font-lock-defaults
	      '(tal-font-lock-keywords
		nil nil ((?_ . "w") (?? . "w") (?$ . "w")) beginning-of-line
		(font-lock-comment-start-regexp . ";")
		(font-lock-mark-block-function . mark-paragraph)))
	(font-lock-mode 1))))

(if (and window-system tal-do-fontify)
    (add-hook 'tal-mode-hook 'tal-fontify))

;;; Major Mode

(defun tal-mode ()
  "Major mode for editing TAL files.
Special commands:
\\{tal-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tal-mode)
  (setq mode-name "TAL")
  (use-local-map tal-mode-map)
  (set-syntax-table tal-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start "[a-zA-Z_$?][a-zA-Z_$?0-9]*[ \\t\\f]*:")
  (setq paragraph-separate "[a-zA-Z_$?][a-zA-Z_$?0-9]*[ \\t\\f]*:[ \\t\\f]*$")
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (setq comment-start-skip ";[ \\t]*")
  (setq comment-start ";")
  (setq comment-end "\n")
  (setq comment-column 40)
  (run-hooks 'tal-mode-hook))

(provide 'tal)

;;; EOF: tal.el
