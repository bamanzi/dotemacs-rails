
;; ** web-mode
(autoload 'web-mode "web-mode"
  "Major mode for editing mixed HTML Templates." t)

(progn
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  )

(when (boundp 'ac-modes)
  (add-to-list 'ac-modes 'web-mode))

;; *** multi-web-mode
(autoload 'multi-web-mode "multi-web-mode"
  "Enables the multi web mode chunk detection and indentation" t)
(autoload 'multi-web-global-mode "multi-web-mode"
  "Toggle Multi-Web mode in every possible buffer." t)

(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")
                  (ruby-mode "<%" "%>")
                  ))
                  
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))

;;NOTE: this is required for `multi-web-mode' to work correctly
;;(multi-web-global-mode 1)


;; ** php
;;use this fork: https://github.com/ejmr/php-mode
(autoload 'php-mode  "php-mode"
  "Major mode for editing PHP code." t)

;;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . php-mode))

;; *** geben debugger
(autoload 'geben "geben"
  "Start GEBEN, a DBGp protocol frontend - a script debugger." t)


;; ** javascript
;; *** espresso
;;http://www.nongnu.org/espresso/
(autoload 'espresso-mode "espresso"
  "Major mode for editing JavaScript source text." t)

;;it's already in gnu emacs 23.2+ (renamed to 'js-mode')
(when (string< emacs-version "23.2")
  (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
  (when (boundp 'ac-modes)
    (add-to-list 'ac-modes 'espresso-mode))
  )

;; *** js2-mode
;;this fork is maintained actively: https://github.com/mooz/js2-mode
;;here we use the emacs-23 branch: https://github.com/mooz/js2-mode/tree/emacs23

(autoload 'js2-mode  "js2-mode"
  "Major mode for editing JavaScript code." t)

;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (if (require 'js2-imenu-extras nil t)
         (js2-imenu-extras-setup)
       (message "WARN: failed to load package `js2-imenu-extras'."))
     ))

;; *** jquery-doc
(autoload 'jquery-doc "jquery-doc"
  "Displays the jquery doc in a buffer." t)


;; ** css
;; *** rainbow-mode: minor mode highlighting color values
(autoload 'rainbow-mode "rainbow-mode"
  "Colorize strings that represent colors." t)

;; *** css-eldoc
(eval-after-load "css-mode"
  `(if (require 'css-eldoc nil t)     
       (add-hook 'css-mode-hook 'css-eldoc-enable)
     (message "WARN: failed to load `css-eldoc' package"))
  )


;; ** misc
;; *** know-your-http-well
(autoload 'http-status-code "http-status-codes"
  "Display the meaning of an HTTP status code or phrase" t)

(autoload 'http-method "http-methods"
  "Display the meaning of an HTTP method" t)

(autoload 'http-header "http-headers"
  "Display the meaning of an HTTP header" t)

(autoload 'css-property "know-your-css-well"
  "Undocumented." t) 

;; *** restclient
(autoload 'restclient-mode "restclient"
  "Major-mode." t)

(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

; from json.el of emacs-24.4
(defun json-pretty-print-buffer ()
  "Pretty-print current buffer."
  (interactive)
  (json-pretty-print (point-min) (point-max)))

;; (defun json-pretty-print (begin end)
;;   "Pretty-print selected region."
;;   (interactive "r")
;;   (atomic-change-group
;;     (let ((json-encoding-pretty-print t)
;;           (txt (delete-and-extract-region begin end)))
;;       (insert (json-encode (json-read-from-string txt))))))

;; a better implementation
(defun json-pretty-print (begin end)
  (interactive "r")
  (shell-command-on-region begin end "python -mjson.tool" nil 'replace))
