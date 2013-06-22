
;;** web-mode
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

;;** php-mode
;;use this fork: https://github.com/ejmr/php-mode
(autoload 'php-mode  "php-mode"
  "Major mode for editing PHP code." t)

;;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . php-mode))


;;** javascript
;;*** espresso
;;http://www.nongnu.org/espresso/
(autoload 'espresso-mode "espresso"
  "Major mode for editing JavaScript source text." t)

;;it's already in gnu emacs 23.2+ (renamed to 'js-mode')
(if (string< emacs-version "23.2")
    (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode)))

;;*** js2-mode
;;this fork is maintained actively: https://github.com/mooz/js2-mode
;;https://github.com/mooz/js2-mode/tree/emacs23

