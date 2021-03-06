;; * common package for ruby web development
;; ** rhtml-mode
(autoload 'rhtml-mode "rhtml-mode"
  "Embedded Ruby Mode (RHTML)" t)

;;TODO: which one is better?
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
;;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; ** haml-mode
(autoload 'haml-mode "haml-mode"
  "Major mode for editing Haml files." t)

(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; ** yaml-mode
(autoload 'yaml-mode  "yaml-mode"
  "Simple mode to edit YAML." t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; * sinatra
(autoload 'sinatra-rake "sinatra"
  "Tab completion selection of a rake task to execute with the" t)
(autoload 'sinatra-console "sinatra"
  "Run script/console in a compilation buffer, with command" t)
(autoload 'sinatra-web-server "sinatra"
  "Run script/server.  Dump output to a compilation buffer" t)

;; * padrino
;; ** inf-ruby support
(eval-after-load "info-ruby"
  `(progn
     (add-to-list 'inf-ruby-console-patterns-alist '("config/apps.rb" . padrino))
    ))

(defun inf-ruby-console-padrino (dir)
  "Run Padino console in DIR."
  (interactive "D")
  (let ((default-directory dir))
    (run-ruby "padrino console" "padrino")))

;; * ruby on rails
;; ** rinari: rails code navigation & utils
;;http://rinari.rubyforge.org/
(autoload 'rinari-launch "rinari"
  "Call function `rinari-minor-mode' if inside a rails project." t)

(autoload 'rinari-console "rinari"
  "Run a Rails console in a compilation buffer." t)

(autoload 'rinari-sql "rinari"
  "Browse the application's database." t)

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'rinari-launch 'append)))

(eval-after-load "web-mode"
  `(progn
     (add-hook 'web-mode-hook 'rinari-launch  'append)))

(eval-after-load "haml-mode"
  `(progn
     (add-hook 'haml-mode-hook 'rinari-launch 'append)))

(eval-after-load "rhtml-mode"
  `(progn
     (add-hook 'rhtml-mode-hook 'rinari-launch 'append)))

(eval-after-load "yaml-mode"
  `(progn
     (add-hook 'yaml-mode-hook 'rinari-launch  'append)))


(eval-after-load "rinari"
  `(progn
     (define-key rinari-minor-mode-map (kbd "C-c .") 'robe-jump)
     ))

(defun rinari.info ()
  "Open Info reader to read rinari.info"
  (interactive)
  (let ((filepath (expand-file-name "info/rinari.info"
                                    (if (boundp 'dotemacs-rails-dir)
                                        dotemacs-rails-dir
                                      default-directory))))
    (info filepath)))

;; this would make `rinari-web-server' happy on emacs-23
(setq compilation-save-buffers-predicate nil)

;; ** projectile-rails: a replacement for rinari
;; https://github.com/asok/projectile-rails
;; advantages over `rinari':
;;   + auto uses `zeus' when run 'rails console' and 'rake'
;;   + `projectile-rails-find-log': open log files with `auto-revert-mode' on

(autoload 'projectile-rails-console "projectile-rails"
  "Undocumented." t)
(defalias 'zeus-console 'projectile-rails-console)

(autoload 'projectile-rails-rake "projectile-rails"
  "Undocumented." t)
(defalias 'zeus-rake    'projectile-rails-rake)

(autoload 'projectile-rails-find-log "projectile-rails"
  "Open Rails log with auto-revert-mode on." t)



;; ** anything-rails
(defun anything-rails-files ()
  "Find file in rails project."
  (interactive)
  (anything-other-buffer '(anything-c-source-rails-project-files)))

(autoload 'rake "anything-rails"
  "Uses Anything to show and execute rake tasks" t)

(defalias 'anything-rake-tasks 'rake)

;; ** rails-log
(autoload 'rails-log-show-development "rails-log-mode"
  "Run tail -f in the development.log file and display the output in a buffer." t)

;; ** misc

(defun eshell/rdebug-rails3 ()
  "Use `realgud-rdebug' to start debugging rails application."
  (interactive)
  (require 'realgud)
  (rdebug "rdebug script/rails -- server"))

;; *** Capitalize keywords in SQL mode
(autoload 'sqlup-mode  "sqlup-mode"
  "Capitalizes SQL keywords for you." t)

(autoload 'sqlup-capitalize-keywords-in-region "sqlup-mode"
  "Call this function on a region to capitalize the SQL keywords therein." t)

(eval-after-load "sql"
  `(progn
     (add-hook 'sql-mode-hook 'sqlup-mode)
     ;; Capitalize keywords in an interactive session (e.g. psql)
     (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
     ;; Set a global keyword to use sqlup on a region
     (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
     ))

