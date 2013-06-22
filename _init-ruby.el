;;* ruby
;;** code folding
(defun bmz/ruby-mode-init-folding ()
  (setq outline-regexp " *\\(def \\|class\\|module\\)")

  (require 'hideshow)
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def\\|do\\)"
                 "end"
                 "#"
                 (lambda (arg) (ruby-end-of-block))
                 nil
                 ))
  )

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'bmz/ruby-mode-init-folding)
     ))


;;** inf-ruby
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
         ))


;;** roby: Code navigation, documentation lookup and completion for Ruby
(autoload 'robe-mode "robe"
  "Improved navigation for Ruby" t)

(add-hook 'ruby-mode-hook 'robe-mode)

(defun ruby-mode-enable-ac ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-robe))

(eval-after-load "ruby-mode"
  `(progn
     (when (and (require 'auto-complete nil t) 
		(require 'robe-ac nil t))
	 (add-hook 'ruby-mode-hook 'ruby-mode-enable-ac))))


;;** rdebug
;;for ruby > 1.9, install 'debugger' gem
;;
;;(add-to-list 'load-path "/usr/lib/ruby/gems/1.9.1/debugger-1.6.0/emacs")
(autoload 'rdebug "rdebug-core"
  "Invoke the rdebug Ruby debugger and start the Emacs user interface." t)
(eval-after-load "rdebug"
  `(progn
    ;;rdebug-core requires package `gdb-ui', but emacs 24 renamed it to gdb-mi
    (unless (locate-library "gdb-ui")
      (require 'gdb-mi)
      (provide 'gdb-ui))))


;;** misc
;;*** ruby-block
(autoload 'ruby-block-mode  "ruby-block"
  "In ruby-mode, Displays the line where there is keyword corresponding" t)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'ruby-block-mode)

;;*** rake
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
