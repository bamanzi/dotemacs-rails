;;; anything-rubygems-local.el --- Installed local rubygems find-file for anything

;; This package is derived from `helm-rubygems-local':
;; {{
;; Copyright (C) 2013 by hadashiA
;; Author: hadashiA <dev@hadashikick.jp>
;; URL: https://github.com/f-kubotar/helm-rubygems-local
;; Version: 20130711.1811
;; X-Original-Version: 0.0.1
;; }
;; URL: http://github.com/bamanzi/anything-rubygems-local
;; Maintainer: Ba Manzi <bamanzi@gmail.com>
;; Package-Requires: ((anything "1.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun anything-c-rubygems-local-init ()
  (let ((gemfile-dir
         (block 'find-gemfile
           (let* ((cur-dir (file-name-directory
                            (expand-file-name (or (buffer-file-name)
                                                  default-directory))))
                  (cnt 0))
             (while (and (< (setq cnt (+ 1 cnt)) 10)
                         (not (equal cur-dir "/")))
               (when (member "Gemfile" (directory-files cur-dir))
                 (return-from 'find-gemfile cur-dir))
               (setq cur-dir (expand-file-name (concat cur-dir "/.."))))
             ))))
    (anything-attrset 'gem-command
                      (if gemfile-dir
                          (format "BUNDLE_GEMFILE=%s/Gemfile bundle" gemfile-dir)
                        "gem"))
    (unless (anything-candidate-buffer)
      (call-process-shell-command (format "%s list 2>/dev/null" (anything-attr 'gem-command))
                                  nil
                                  (anything-candidate-buffer 'local)))))

(defun anything-c-rubygems-local-action (choice)
  (let* ((gem-name  (if (string-match "[\s\*]*\\([a-z0-9\._-]+\\)\s.+" choice)
                        (match-string 1 choice)
                      choice))         
         (gem-which (replace-regexp-in-string "\n$" ""
                                              (shell-command-to-string
                                               (format "%s %s"
                                                       (if (string= (anything-attr 'gem-command) "gem")
                                                           "gem which"
                                                         "bundle show")
                                                       gem-name))))
         (path))
    (message "open gem '%s': %s" gem-name gem-which)
    (if (or (null gem-which)
            (string= "" gem-which)
            (string-match "^ERROR:" gem-which))
        (message "Can't find ruby library file or shared library %s" gem-name)
      (setq path (if (string= (anything-attr 'gem-command) "gem")
                     (file-name-directory gem-which)
                   gem-which))  ;;`bundle show gem` returns folder, while `gem which` return rb path
      (if (and path (file-exists-p path))
          (find-file path)
        (message "no such file or directory: \"%s\"" path))
      )
    ))

(defvar anything-c-source-rubygems-local
  '((name . "rubygems")
    (candidates-in-buffer)
    (init . anything-c-rubygems-local-init)
    (action . anything-c-rubygems-local-action)
    ))

;;;###autoload
(defun anything-rubygems-local ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-rubygems-local)
   "*anything local gems*"
  ))

(provide 'anything-rubygems-local)

;;; anything-rubygems-local.el ends here
