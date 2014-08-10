;; * ruby
;; ** major mode
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

;; ** code folding
(defun ruby-mode-init-folding ()
  (interactive)

  (setq outline-regexp " *\\(def \\|class \\|module \\)")


  (require 'hideshow)
  (let ((settings '("\\<\\(def\\|class\\|module\\|begin\\|do\\)\\>"
                    "end"
                    "#"
                    (lambda (arg) (ruby-end-of-block))
                    nil
                    )))
    (if (assoc 'ruby-mode hs-special-modes-alist)
        (setcdr (assoc 'ruby-mode hs-special-modes-alist) settings)
      (add-to-list 'hs-special-modes-alist (cons ruby-mode settings))))
    

  (let ((settings '("\\<\\(def\\|class\\|module\\|begin\\|do\\)\\>"
                    "end"
                    "#"
                    (lambda (arg) (enh-ruby-end-of-block))
                    nil
                    )))
    (if (assoc 'enh-ruby-mode hs-special-modes-alist)
        (setcdr (assoc 'enh-ruby-mode hs-special-modes-alist) settings)
      (add-to-list 'hs-special-modes-alist (cons enh-ruby-mode settings))))
  
  )
  
(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'ruby-mode-init-folding)
     ))


;; ** inf-ruby
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")


(defun ruby-mode-init-inf-ruby ()  
  (when (require 'inf-ruby nil t)
    (cond
     ((fboundp 'inf-ruby-minor-mode)
      (inf-ruby-minor-mode 1))
     ((fboundp 'inf-ruby-setup-keybindings)
       (inf-ruby-setup-keybindings))
     ((fboundp 'inf-ruby-keys) ;;older version of inf-ruby
	  (inf-ruby-keys))
     (t
      (message "how to activate inf-ruby?")))))

(add-hook 'ruby-mode-hook 'ruby-mode-init-inf-ruby)

(eval-after-load "inf-ruby"
  `(if (< emacs-major-version 24)
       ;; backport from emacs-24.3
       (defun locate-dominating-file (file name)
         "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
         ;; We used to use the above locate-dominating-files code, but the
         ;; directory-files call is very costly, so we're much better off doing
         ;; multiple calls using the code in here.
         ;;
         ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
         ;; `name' in /home or in /.
         (setq file (abbreviate-file-name file))
         (let ((root nil)
               ;; `user' is not initialized outside the loop because
               ;; `file' may not exist, so we may have to walk up part of the
               ;; hierarchy before we find the "initial UID".  Note: currently unused
               ;; (user nil)
               try)
           (while (not (or root
                           (null file)
                           ;; FIXME: Disabled this heuristic because it is sometimes
                           ;; inappropriate.
                           ;; As a heuristic, we stop looking up the hierarchy of
                           ;; directories as soon as we find a directory belonging
                           ;; to another user.  This should save us from looking in
                           ;; things like /net and /afs.  This assumes that all the
                           ;; files inside a project belong to the same user.
                           ;; (let ((prev-user user))
                           ;;   (setq user (nth 2 (file-attributes file)))
                           ;;   (and prev-user (not (equal user prev-user))))
                           (string-match locate-dominating-stop-dir-regexp file)))
             (setq try (if (stringp name)
                           (file-exists-p (expand-file-name name file))
                         (funcall name file)))
             (cond (try (setq root file))
                   ((equal file (setq file (file-name-directory
                                            (directory-file-name file))))
                    (setq file nil))))
           (if root (file-name-as-directory root))))
   ))

;; *** pry
(autoload 'run-pry "pry"
  "Run an inferior Pry process, input and output via buffer *pry*." t)


;; ** robe: Code navigation, documentation lookup and completion for Ruby

(autoload 'robe-mode "robe"
  "Improved navigation for Ruby" t)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-to-list 'ruby-mode-hook 'turn-on-eldoc-mode)

;; *** enable auto-complete
(defun ruby-mode-enable-ac ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-robe))

(eval-after-load "robe"
  `(progn
     (define-key ruby-mode-map (kbd "C-c .") 'robe-jump)
     
     (define-key robe-mode-map (kbd "M-.") nil)
     (define-key robe-mode-map (kbd "C-c .") 'robe-jump)

     (if (< emacs-major-version 24)
         ;; backport from emacs-24.3
         (defun process-live-p (process)
           "Returns non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'."
           (memq (process-status process)
                 '(run open listen connect stop)))
       )
     ))

(eval-after-load "ruby-mode"
  `(progn
     
     (when (and (require 'auto-complete nil t) 
                (require 'robe-ac nil t))
       ;;(add-hook 'ruby-mode-hook 'ruby-mode-enable-ac)
       (message "By default `ac-source-robe' disabled in ruby-mode, call `ruby-mode-enable-ac' to enable it.")
       )))

;; *** zossima: a lightweight robe
;;(actually, robe was forked from zossima)
(autoload 'zossima-mode "zossima"
  "Improved navigation for Ruby" t)
;;(add-hook 'ruby-mode-hook 'zossima-mode)



;; ** rdebug
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

;; *** helper to show source code when debugging in inf-ruby
(eval-after-load "inf-ruby"
  `(progn
     (load-library "ruby-debug")
     
     (define-key inf-ruby-mode-map (kbd "C-c C-s") 'rubyd-debug-activate)
     (define-key inf-ruby-mode-map (kbd "C-c C-n") 'rubyd-debug-deactivate)
     
     ))

;; ** syntax checking
;; *** enh-ruby-mode built-in
;; *** flycheck
;; *** rubocop
(autoload 'rubocop-check-current-file "rubocop"
  "Run on current file." t)


;; ** misc
;; *** ruby-block
(autoload 'ruby-block-mode  "ruby-block"
  "In ruby-mode, Displays the line where there is keyword corresponding" t)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'ruby-block-mode)

;; *** rake
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; *** ruby-tools: string/symbol conversion
(eval-after-load "ruby-mode"
  `(progn
     (require 'ruby-tools nil t)))

;; *** speedbar
(eval-after-load "speedbar"
  `(progn
     (speedbar-add-supported-extension ".rb")
     (speedbar-add-supported-extension ".erb")
     (speedbar-add-supported-extension ".yml")
     (speedbar-add-supported-extension ".rake")
     ))

;; *** which-func
(eval-after-load "which-func"
  `(progn
     (add-to-list 'which-func-modes 'ruby-mode)
     (add-to-list 'which-func-modes 'enh-ruby-mode)
     ))

;; *** milkode (similar to grep but with pre-index feature)
(autoload 'milkode:search "milkode"
  "Milkode search current package using `M-x grep`" t)

(autoload 'milkode:search-from-all-packages "milkode"
  "Milkode search all registered packages using `M-x grep`" t)

(define-key search-map (kbd "g m") 'milkode:search)
(define-key search-map (kbd "g M") 'milkode:search-from-all-packages)

;; *** bundler
(autoload 'bundle-open "bundler"
  "Queries for a gem name and opens the location of the gem in dired." t)

(defalias 'find-gem 'bundle-open)
