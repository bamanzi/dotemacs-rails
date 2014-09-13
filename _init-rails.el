;; * ruby on rails
;; ** rhtml-mode for editing html template
(autoload 'rhtml-mode "rhtml-mode"
  "Embedded Ruby Mode (RHTML)" t)

;;TODO: which one is better?
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
;;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))


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
     (add-hook 'ruby-mode-hook 'rinari-launch)))

(eval-after-load "web-mode"
  `(progn
     (add-hook 'web-mode-hook 'rinari-launch)))

(eval-after-load "rhtml-mode"
  `(progn
     (add-hook 'rhtml-mode-hook 'rinari-launch)))

(eval-after-load "yaml-mode"
  `(progn
     (add-hook 'yaml-mode-hook 'rinari-launch)))


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


;; ** yaml-mode
(autoload 'yaml-mode  "yaml-mode"
  "Simple mode to edit YAML." t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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
