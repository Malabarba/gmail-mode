;;; gmail-message-mode.el --- A major-mode for editing gmail messages using markdown syntax.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/gmail-message-mode
;; Version: 1.1
;; Package-Requires: ((ham-mode "1.0"))
;; Keywords: mail convenience emulation
;; Prefix: gmail-message-mode
;; Separator: -

;;; Commentary:
;;
;; gmail-message-mode
;; ==========
;; 
;; **gmail-message-mode** is an emacs major-mode for editing gmail
;; messages using markdown syntax, it is meant for use with browser
;; plugins which allow you to edit text fields with external applications
;; (in this case, emacs). See [Plugins][] below for a list for each
;; browser.
;; 
;; **The problem:** Lately, gmail messages have been demanding html. That
;;   made it very hard to edit them outside your browser, because you had
;;   to edit html source code (for instance, linebreaks were ignored and
;;   you had to type `<br>' instead).
;;   
;; **gmail-message-mode to the rescue:** Simply activate this mode in
;;   gmail messages (See [Activation][]); the buffer is converted to
;;   markdown and you may edit at will, but the file is still saved as
;;   html behind the scenes so GMail won't know a thing! *See
;;   [ham-mode][1] to understand how this works.*
;;   
;; Activation
;; ----------
;; Make sure you install it:
;; 
;;     M-x package-install RET gmail-message-mode
;;     
;; And that's it!  
;; *(if you install manually, note that it depends on [ham-mode][1])*
;; 
;; This package will (using `auto-mode-alist') configure emacs to
;; activate `gmail-message-mode' whenever you're editing a file that
;; seems to be a gmail message. However, given the wide range of possible
;; plugins, it's hard to catch them all. You may have to add entries
;; manually to `auto-mode-alist', to make sure `gmail-message-mode' is
;; activated.
;; 
;; Here's an example. One of the lines in `gmail-message-mode-activate'
;; is:
;; 
;; ## Plugins ##
;; 
;; 1. **Firefox** - [It's all text][] combined with [Old Compose][] (see [this thread][] on why you need the second).
;; 2. **Google-Chrome** - [Several][]
;; 3. **Conkeror** - [Spawn Helper (built-in)][]
;; 
;; 
;; [Activation]: #activation
;; 
;; [Plugins]: #plugins
;; 
;; [It's all text]: https://addons.mozilla.org/en-US/firefox/addon/its-all-text/
;; 
;; [Several]: http://superuser.com/questions/261689/its-all-text-for-chrome
;; 
;; [Spawn Helper (built-in)]: http://conkeror.org/ConkerorSpawnHelper
;; 
;; [this thread]: http://github.com/docwhat/itsalltext
;; 
;; [Old Compose]: http://oldcompose.com/
;; 
;; [1]: https://github.com/Bruce-Connor/ham-mode

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
;; 1.1   - 2013/12/09 - gmail-message-mode-signature-properties can hide the signature.
;; 1.0.1 - 2013/12/07 - gmail-message-mode--blockquote.
;; 1.0   - 2013/12/05 - Created File.
;;; Code:

(defconst gmail-message-mode-version "1.1" "Version of the gmail-message-mode.el package.")
(defconst gmail-message-mode-version-int 3 "Version of the gmail-message-mode.el package, as an integer.")
(defun gmail-message-mode-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and gmail-message-mode versions."
  (interactive)
  (message "Your gmail-message-mode-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           gmail-message-mode-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/gmail-message-mode/issues/new"))

;;;###autoload
(defcustom gmail-message-mode-auto-mode-list
  '("mail.google.com.*.\\(ckr\\|html?\\|txt\\)\\'" ;conkeror and other stuff
    ".*[\\\\/]itsalltext[\\\\/]mail\.google\..*\\'" ;it's all text
    )
  "List of regexps which will be added to `auto-mode-alist' (associated to `gmail-message-mode').

If the file path matches any of these, `gmail-message-mode' will be
activated on the current file.

If you don't want `gmail-message-mode' to add itself to your
`auto-mode-alist' simply set this variable to nil.

If you add items manually (not through the customization
interface), you'll need to call `gmail-message-mode--set-amlist' for it
to take effect.
Removing items only takes effect after restarting Emacs."
  :type '(repeat regexp)
  :group 'gmail-message-mode
  :set 'gmail-message-mode--set-amlist
  :initialize 'custom-initialize-default
  :package-version '(gmail-message-mode . "1.0"))

(defun gmail-message-mode-save-finish-suspend ()
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

(defvar gmail-message-mode--blockquote
  (concat "<blockquote style=\"margin: 0px 0px 0px 0.8ex;"
          " border-left: 1px solid rgb(204, 204, 204);"
          " padding-left: 1ex;"
          "\" class=\"gmail_quote\">"))

(defun gmail-message-mode--fix-tags (file)
  "Fix special tags for gmail, such as blockquote."
  (let ((newContents
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (while (search-forward "<blockquote>" nil t)
             (replace-match gmail-message-mode--blockquote :fixedcase :literal))
           (buffer-string))))
    (write-region newContents nil file nil t)))

;;;###autoload
(define-derived-mode gmail-message-mode ham-mode "GMail"
  "Designed for GMail messages. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.
\\<gmail-message-mode-map>
Also defines a key \\[gmail-message-mode-save-finish-suspend] for `gmail-message-mode-save-finish-suspend'.

\\{gmail-message-mode-map}"
  :group 'gmail-message-mode
  (add-hook 'ham-mode-md2html-hook 'gmail-message-mode--fix-tags :local)
  (gmail-message-mode--propertize-buffer))

(defvar gmail-message-mode--end-regexp
  "<br *clear=\"all\">\\|<div><div *class=\"gmail_extra\">\\|<div *class=\"gmail_extra\">"
  "Regexp defining where a message ends and signature or quote starts.")

(defcustom gmail-message-mode-signature-properties
  `(display ,(if (char-displayable-p ?…) "..." "…")
            intangible t
            read-only t
            pointer arrow
            mouse-face mode-line-highlight
            keymap ,(let ((map (make-sparse-keymap)))
                      (define-key map "\C-j" 'gmail-message-mode--expand-end)
                      (define-key map "\C-i" 'gmail-message-mode--expand-end)
                      (define-key map [return] 'gmail-message-mode--expand-end)
                      map))
  "Property list to use on the signature.

Does not affect the final e-mail. This is just used to hide
useless stuff from the user."
  :type '(repeat symbol (choice symbol string))
  :group 'gmail-message-mode
  :package-version '(gmail-message-mode . "1.0.1"))

(defun gmail-message-mode--expand-end ()
  "Expand the ending of the message, if it was collapsed."
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties
     (point-min) (point-max)
     gmail-message-mode-signature-properties)))

(defun gmail-message-mode--propertize-buffer ()
  "Add some text properties to the buffer, like coloring the signature."
  (goto-char (point-min))
  (when (search-forward-regexp gmail-message-mode--end-regexp nil :noerror)
    (add-text-properties (match-beginning 0) (point-max)
                         gmail-message-mode-signature-properties)))

(define-key gmail-message-mode-map (kbd "C-c C-z") 'gmail-message-mode-save-finish-suspend)

;;;###autoload
(defun gmail-message-mode--set-amlist (&optional sym val)
  "Reset the auto-mode-alist."
  (when sym
    (set-default sym val))
  (mapc
   (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode)))
   gmail-message-mode-auto-mode-list))
;;;###autoload
(mapc
 (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode)))
 gmail-message-mode-auto-mode-list)

(provide 'gmail-message-mode)
;;; gmail-message-mode.el ends here.
