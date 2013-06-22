;;* ruby on rails
;;** rhtml-mode for editing html template
(autoload 'rhtml-mode "rhtml-mode"
  "Embedded Ruby Mode (RHTML)" t)

;;TODO: which one is better?
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
;;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))


;;** rinari: rails code navigation & utils
;;http://rinari.rubyforge.org/
(autoload 'rinari-launch "rinari"
  "Call function `rinari-minor-mode' if inside a rails project." t)

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

;;** yaml-mode
(autoload 'yaml-mode  "yaml-mode"
  "Simple mode to edit YAML." t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
