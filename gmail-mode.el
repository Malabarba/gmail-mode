;;; gmail-mode.el --- A major-mode for editing gmail messages using markdown syntax.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/gmail-mode
;; Version: 1.0.1
;; Package-Requires: ((ham-mode "1.0"))
;; Keywords: mail convenience emulation
;; Prefix: gmail-mode
;; Separator: -

;;; Commentary:
;;
;; **gmail-mode** is an emacs major-mode for editing gmail messages
;; using markdown syntax, it is meant for use with browser plugins
;; which allow you to edit text fields with external applications (in
;; this case, emacs). See [Plugins][] below for a list for each
;; browser.
;; 
;; *The problem:* Lately, gmail messages have been demanding html.
;; That made it very hard to edit them outside your browser, because
;; you had to edit html source code (for instance, linebreaks were
;; ignored and you had to type `<br>' instead).
;; 
;; *gmail-mode to the rescue:* Simply activate this mode in gmail
;; messages (See [Activation][]); the buffer is converted to markdown
;; and you may edit at will, but the file is still saved as html
;; behind the scenes so GMail won't know a thing! *See [ham-mode][1]
;; to understand how this works.*
;; 
;; Activation
;; ==========
;; Make sure you install it:
;; 
;;     M-x package-install RET gmail-mode
;; 
;; And that's it!  
;; *(if you install manually, note that it depends on [ham-mode][1])*
;; 
;; This package will (using `auto-mode-alist') configure emacs to
;; activate `gmail-mode' whenever you're editing a file that seems to be
;; a gmail message. However, given the wide range of possible plugins,
;; it's hard to catch them all. You may have to add entries manually to
;; `auto-mode-alist', to make sure `gmail-mode' is activated.
;; 
;; Here's an example. One of the lines in `gmail-mode-activate' is:
;; 
;; ## Plugins ##
;; 
;; 1. **Firefox** - [It's all text][] combined with [Old Compose][] (see
;;    [this thread][] on why you need the second).
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
;; 1.0.1 - 2013/12/07 - gmail-mode--blockquote.
;; 1.0   - 2013/12/05 - Created File.
;;; Code:

(defconst gmail-mode-version "1.0.1" "Version of the gmail-mode.el package.")
(defconst gmail-mode-version-int 2 "Version of the gmail-mode.el package, as an integer.")
(defun gmail-mode-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and gmail-mode versions."
  (interactive)
  (message "Your gmail-mode-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           gmail-mode-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/gmail-mode/issues/new"))

;;;###autoload
(defcustom gmail-mode-auto-mode-list
  '("mail.google.com.*.\\(ckr\\|html?\\|txt\\)\\'" ;conkeror and other stuff
    ".*[\\\\/]itsalltext[\\\\/]mail\.google\..*\\'" ;it's all text
    )
  "List of regexps which will be added to `auto-mode-alist' (associated to `gmail-mode').

If the file path matches any of these, `gmail-mode' will be
activated on the current file.

If you don't want `gmail-mode' to add itself to your
`auto-mode-alist' simply set this variable to nil.

If you add items manually (not through the customization
interface), you'll need to call `gmail-mode--set-amlist' for it
to take effect.
Removing items only takes effect after restarting Emacs."
  :type '(repeat regexp)
  :group 'gmail-mode
  :set 'gmail-mode--set-amlist
  :initialize 'custom-initialize-default
  :package-version '(gmail-mode . "1.0"))

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

(defvar gmail-mode--blockquote
  (concat "<blockquote style=\"margin: 0px 0px 0px 0.8ex;"
          " border-left: 1px solid rgb(204, 204, 204);"
          " padding-left: 1ex;"
          "\" class=\"gmail_quote\">"))

(defun gmail-mode--fix-tags (file)
  "Fix special tags for gmail, such as blockquote."
  (let ((newContents
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (while (search-forward "<blockquote>" nil t)
             (replace-match gmail-mode--blockquote :fixedcase :literal))
           (buffer-string))))
    (write-region newContents nil file nil t)))

;;;###autoload
(define-derived-mode gmail-mode ham-mode "GMail"
  "Designed for GMail messages. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.
\\<gmail-mode-map>
Also defines a key \\[gmail-mode-save-finish-suspend] for `gmail-mode-save-finish-suspend'.

\\{gmail-mode-map}"
  :group 'gmail-mode
  (add-hook 'ham-mode-md2html-hook 'gmail-mode--fix-tags :local))

(define-key gmail-mode-map (kbd "C-c C-z") 'gmail-mode-save-finish-suspend)

;;;###autoload
(defun gmail-mode--set-amlist (&optional sym val)
  "Reset the auto-mode-alist."
  (when sym
    (set-default sym val))
  (mapc
   (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-mode)))
   gmail-mode-auto-mode-list))
;;;###autoload
(mapc
 (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-mode)))
 gmail-mode-auto-mode-list)

(provide 'gmail-mode)
;;; gmail-mode.el ends here.
