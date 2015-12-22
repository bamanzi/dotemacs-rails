;; * ruby
;; ** rvm
(autoload 'rvm-use "rvm"
  "switch the current ruby version to any ruby, which is installed with rvm" t)

(autoload 'rvm-use-default "rvm"
  "use the rvm-default ruby as the current ruby version" t)


;; ** major mode
(defun ruby-set-major-mode (mode)
  (interactive (list
                (intern (completing-read "Associate ruby files to major mode: "
                                         '("ruby-mode" "enh-ruby-mode")
                                         nil
                                         'match))))
  (mapc #'(lambda (ext)
            (let ((pair (assoc ext auto-mode-alist)))
              (if pair
                  (setcdr pair mode)
                (add-to-list 'auto-mode-alist (cons ext mode)))))
        '("\\.rb\\'"                    ; enh-ruby-mode would set this
          "Rakefile\\'"                 ; enh-ruby-mode would set this
          "\\.gemspec\\'"               ; enh-ruby-mode would set this
          "\\.rake\\'"
          "Gemfile\\'"
          "\\.ru\\'"
          "Vagrantfile\\'"))
  (message "Ruby files now associated to major mode `%s'. Call `M-x ruby-set-major-mode` to change it." mode))

(when (boundp 'ac-modes)
  (add-to-list 'ac-modes 'ruby-mode)
  (add-to-list 'ac-modes 'enh-ruby-mode))

;; *** ruby-mode
(ruby-set-major-mode 'ruby-mode)

(setq ruby-deep-indent-paren t)

;; *** enh-ruby-mode
;; ruby > 1.9.1 required
(autoload 'enh-ruby-mode  "enh-ruby-mode"
  "Enhanced Major mode for editing Ruby code." t)

(eval-after-load "enh-ruby-mode"
  `(progn
     ;; copy all hooks
     (mapc #'(lambda (func)
               (add-hook 'enh-ruby-mode-hook func))
           ruby-mode-hook)

     ;; file associations
     (ruby-set-major-mode 'enh-ruby-mode)
     ))

;; ** code folding
(defun ruby-mode-init-folding ()
  (interactive)

  (setq outline-regexp " *\\(def \\|class \\|module \\)")
  )

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'ruby-mode-init-folding)
     ))

(eval-after-load "ruby-mode"
  `(progn
     (require 'hideshow)
     
     (let ((settings '("\\<\\(def\\|class\\|module\\|begin\\|do\\)\\>"
                       "end"
                       "#"
                       (lambda (arg) (ruby-end-of-block))
                    nil
                    )))
       (if (assoc 'ruby-mode hs-special-modes-alist)
           (setcdr (assoc 'ruby-mode hs-special-modes-alist) settings)
         (add-to-list 'hs-special-modes-alist (cons 'ruby-mode settings))))
     ))

(eval-after-load "enh-ruby-mode"
  `(progn
     (require 'hideshow)

     (let ((settings '("\\<\\(def\\|class\\|module\\|begin\\|if\\|unless\\|do\\)\\>"
                       "end"
                       "#"
                       (lambda (arg) (enh-ruby-end-of-block))
                       nil
                       )))
       (if (assoc 'enh-ruby-mode hs-special-modes-alist)
           (setcdr (assoc 'enh-ruby-mode hs-special-modes-alist) settings)
         (add-to-list 'hs-special-modes-alist (cons 'enh-ruby-mode settings))))  
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

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'ruby-mode-init-inf-ruby)
     ))

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

(defun pry ()
  "A simple pry REPL."
  (interactive)
  (inf-ruby "pry"))

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

;; ** documentation lookup
(autoload 'yari "yari"
  "Look up Ruby documentation with `ri'." t)

(autoload 'yari-anything "yari"
  "Undocumented." t)

(autoload 'yari-helm "yari"
  "Undocumented." t)

(eval-after-load "ruby-mode"
  `(progn
     (define-key ruby-mode-map (kbd "C-c <f1>") 'yari-anything)
     ))

(eval-after-load "enh-ruby-mode"
  `(progn
     (define-key enh-ruby-mode-map (kbd "C-c <f1>") 'yari-anything)
     ))


;; ** debugging
;; *** ruby-debug https://github.com/denofevil/ruby-debug/tree/master/emacs
(autoload 'rdebug "rdebug-core"
  "Invoke the rdebug Ruby debugger and start the Emacs user interface." t)

(eval-after-load "rdebug"
  `(progn
    ;;rdebug-core requires package `gdb-ui', but emacs 24 renamed it to gdb-mi
    (unless (locate-library "gdb-ui")
      (require 'gdb-mi)
      (provide 'gdb-ui))

    ;; make GUD menu works
    (add-hook 'rdebug-mode-hook 'rdebug-init-gud-commands)
    ))

(defun rdebug-init-gud-commands ()
  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
;;  (gud-def gud-remove "clear %f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "step"         "\C-s" "Step one source line with display.")
  (gud-def gud-next   "next"         "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "continue"     "\C-r" "Continue with display.")
  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
  (gud-def gud-up     "up"           "<" "Up one stack frame.")
  (gud-def gud-down   "down"         ">" "Down one stack frame.")
  (gud-def gud-print  "p %e"         "\C-p" "Evaluate Ruby expression at point.")
  ;; Is this right?
  (gud-def gud-statement "eval %e"      "\C-e" "Execute Ruby statement at point.")
  )

;; *** realgud https://github.com/rocky/emacs-dbgr

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


;; ** code editing
;; *** ruby-block
(autoload 'ruby-block-mode  "ruby-block"
  "In ruby-mode, Displays the line where there is keyword corresponding" t)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'ruby-block-mode)

;; *** ruby-tools: string/symbol conversion
(eval-after-load "ruby-mode"
  `(progn
     (require 'ruby-tools nil t)))

;; *** ruby-hash-syntax
(autoload 'ruby-toggle-hash-syntax "ruby-hash-syntax"
  "Toggle syntax of ruby hash literal in region from BEG to END between ruby 1.8 and 1.9 styles." t)

;; *** which-func
(eval-after-load "which-func"
  `(progn
     (add-to-list 'which-func-modes 'ruby-mode)
     (add-to-list 'which-func-modes 'enh-ruby-mode)
     ))

;; *** ctags
(eval-after-load "anything-config"
  `(progn
     (add-to-list 'anything-c-ctags-modes 'ruby-mode)
     (add-to-list 'anything-c-ctags-modes 'enh-ruby-mode)
     (add-to-list 'anything-c-ctags-modes 'js-mode)
     ))


;; ** misc

;; *** rake
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

(autoload 'rake "rake"
  "Runs rake command." t)

;; *** speedbar
(eval-after-load "speedbar"
  `(progn
     (speedbar-add-supported-extension ".rb")
     (speedbar-add-supported-extension ".erb")
     (speedbar-add-supported-extension ".yml")
     (speedbar-add-supported-extension ".rake")
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

;;(defalias 'find-gem 'bundle-open)

(autoload 'anything-rubygems-local "anything-rubygems-local"
  "Undocumented." t)

(defalias 'find-gem 'anything-rubygems-local)

;; *** cucumber
(autoload 'feature-mode "feature-mode"
  "Major mode for editing plain text stories" t)

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

