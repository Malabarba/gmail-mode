;;; gmail-mode.el --- A major-mode for editing gmail messages using markdown syntax.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/gmail-mode
;; Version: 1.0
;; Package-Requires: ((html-to-markdown "1.1"))
;; Keywords: mail convenience emulation
;; Prefix: gmail-mode
;; Separator: -

;;; Commentary:
;;
;; 

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
;;
;;     (require 'gmail-mode)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 1.0 - 2013/12/05 - Created File.
;;; Code:

(defconst gmail-mode-version "1.0" "Version of the gmail-mode.el package.")
(defconst gmail-mode-version-int 1 "Version of the gmail-mode.el package, as an integer.")
(defun gmail-mode-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and gmail-mode versions."
  (interactive)
  (message "Your gmail-mode-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           gmail-mode-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/gmail-mode/issues/new"))

(defun gmail-mode-save-finish-suspend ()
  "Save the buffer as html, call `server-edit', and suspend the emacs frame.

This command is used for finishing your edits. It'll do all the
buffer needs and then send emacs to the background so that the web
browser can take focus automatically."
  (interactive)
  (save-buffer)
  (if (frame-parameter nil 'client)
      (server-edit)
    (message "Not in a client buffer, won't call `server-edit'."))
  (if (and window-system (not (eq window-system 'pc)))
      (suspend-frame)
    (message "Not in a graphical frame, won't call `suspend-frame'.")))

;;;###autoload
(define-derived-mode gmail-mode ham-mode "GMail"
  "Designed for GMail messages. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.
\\<gmail-mode-map>
Also defines a key \\[gmail-mode-save-finish-suspend] for `gmail-mode-save-finish-suspend'.

\\{gmail-mode-map}"
  :group 'gmail-mode)

(define-key gmail-mode-map (kbd "C-c C-z") 'gmail-mode-save-finish-suspend)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("mail-google-com.*.\\(ckr\\|htm\\)\\'" . gmail-mode))
  (add-to-list 'auto-mode-alist '(".*/itsalltext/mail\.google\..*\\'" . gmail-mode)))


(provide 'gmail-mode)
;;; gmail-mode.el ends here.
