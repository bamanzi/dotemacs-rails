;;* ruby
;;** major mode
(autoload 'enh-ruby-mode  "enh-ruby-mode"
  "Enhanced Major mode for editing Ruby code." t)

(eval-after-load "enh-ruby-mode"
  `(progn
     ;; copy all hooks
     (mapc #'(lambda (func)
               (add-hook 'enh-ruby-mode-hook func))
           ruby-mode-hook)

     ;; file associations
     (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
     (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
     ))

;;** code folding
(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook
               #'(lambda ()
                   (setq outline-regexp " *\\(def \\|class \\|module \\)")
                   ))

     (require 'hideshow)
     (add-to-list 'hs-special-modes-alist
                  '(ruby-mode
                    "\\(def \\|class \\|module \\)"
                    "end"
                    "#"
                    (lambda (arg) (ruby-end-of-block))
                    nil
                    ))
     
     ))


;;** inf-ruby
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")


(defun ruby-mode-init-inf-ruby ()
  (when (require 'inf-ruby nil t)
    (if (fboundp 'inf-ruby-setup-keybindings)
	(inf-ruby-setup-keybindings)
      (if (fboundp 'inf-ruby-keys) ;;older version of inf-ruby
	  (inf-ruby-keys)))))

(add-hook 'ruby-mode-hook 'ruby-mode-init-inf-ruby)


;;** robe: Code navigation, documentation lookup and completion for Ruby

(autoload 'robe-mode "robe"
  "Improved navigation for Ruby" t)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-to-list 'ruby-mode-hook 'turn-on-eldoc-mode)

;;*** enable auto-complete
(defun ruby-mode-enable-ac ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-robe))

(eval-after-load "robe"
  `(progn
     (define-key robe-mode-map (kbd "M-.") nil)
     (define-key robe-mode-map (kbd "C-c .") 'robe-jump)
     ))

(eval-after-load "ruby-mode"
  `(progn
     
     (when (and (require 'auto-complete nil t) 
		(require 'robe-ac nil t))
	 ;;(add-hook 'ruby-mode-hook 'ruby-mode-enable-ac)
       )))

;;*** zossima: a lightweight robe
;;(actually, robe was forked from zossima)
(autoload 'zossima-mode "zossima"
  "Improved navigation for Ruby" t)
;;(add-hook 'ruby-mode-hook 'zossima-mode)



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

;;*** ruby-tools: string/symbol conversion
(eval-after-load "ruby-mode"
  `(progn
     (require 'ruby-tools nil t)))

;;*** speedbar
(eval-after-load "speedbar"
  `(progn
     (speedbar-add-supported-extension ".rb")
     (speedbar-add-supported-extension ".erb")
     (speedbar-add-supported-extension ".yml")
     (speedbar-add-supported-extension ".rake")
     ))

;;*** rubocop
(autoload 'rubocop-check-current-file "rubocop"
  "Run on current file." t)

;;*** which-func
(eval-after-load "which-func"
  `(progn
     (add-to-list 'which-func-modes 'ruby-mode)
     (add-to-list 'which-func-modes 'enh-ruby-mode)
     ))
