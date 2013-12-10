;;; gmail-message-mode.el --- A major-mode for editing gmail messages using markdown syntax.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/gmail-message-mode
;; Version: 1.3
;; Package-Requires: ((ham-mode "1.0"))
;; Keywords: mail convenience emulation
;; Prefix: gmm/
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
;; 1.3   - 2013/12/10 - Support for edit-server (from chrome).
;; 1.2   - 2013/12/10 - BREAKING CHANGES. Renamed a bunch of stuff.
;; 1.1   - 2013/12/09 - gmm/signature-properties can hide the signature.
;; 1.0.1 - 2013/12/07 - gmm/-blockquote.
;; 1.0   - 2013/12/05 - Created File.
;;; Code:
(require 'ham-mode)

(defconst gmail-message-mode-version "1.3" "Version of the gmail-message-mode.el package.")
(defconst gmail-message-mode-version-int 6 "Version of the gmail-message-mode.el package, as an integer.")
(defun gmail-message-mode-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and gmail-message-mode versions."
  (interactive)
  (message "Your gmail-message-mode-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           gmail-message-mode-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/gmail-message-mode/issues/new"))

;;;###autoload
(defcustom gmm/auto-mode-list
  '("[\\\\/]mail-google-com.*\\.\\(ckr\\|html?\\|txt\\)\\'" ;conkeror and other stuff
    ".*[\\\\/]itsalltext[\\\\/]mail\\.google\\..*\\.txt\\'" ;it's all text
    ".*[\\\\/]-gmm-mirror-[0-9]\\{5\\}\\.html\\'" ;Ourselves
    )
  "List of regexps which will be added to `auto-mode-alist' (associated to `gmail-message-mode').

If the file path matches any of these, `gmail-message-mode' will be
activated on the current file.

If you don't want `gmail-message-mode' to add itself to your
`auto-mode-alist' simply set this variable to nil.

If you add items manually (not through the customization
interface), you'll need to call `gmm/-set-amlist' for it
to take effect.
Removing items only takes effect after restarting Emacs."
  :type '(repeat regexp)
  :group 'gmail-message-mode
  :set 'gmm/-set-amlist
  :initialize 'custom-initialize-default
  :package-version '(gmail-message-mode . "1.0"))

(defun gmm/save-finish-suspend ()
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

(defvar gmm/-blockquote
  (concat "<blockquote style=\"margin: 0px 0px 0px 0.8ex;"
          " border-left: 1px solid rgb(204, 204, 204);"
          " padding-left: 1ex;"
          "\" class=\"gmail_quote\">"))

(defvar gmm/-mirrored-file nil
  "Temporary file used to generate the content of edit-server buffers.

Necessary because edit-server doesn't use actual files to
communicate with chrome.")
(make-variable-buffer-local 'gmm/-mirrored-file)

(defun gmm/-fix-tags (file)
  "Fix special tags for gmail, such as blockquote."
  (let ((newContents
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (while (search-forward "<blockquote>" nil t)
             (replace-match gmm/-blockquote :fixedcase :literal))
           (buffer-string))))
    (write-region newContents nil file nil t)))

;;;###autoload
(define-derived-mode gmail-message-mode ham-mode "GMail"
  "Designed for GMail messages. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.
\\<gmail-message-mode-map>
Also defines a key \\[gmm/save-finish-suspend] for `gmm/save-finish-suspend'.

\\{gmail-message-mode-map}"
  :group 'gmail-message-mode
  (add-hook 'ham-mode-md2html-hook 'gmm/-fix-tags :local)
  (gmm/-propertize-buffer))

;;;###autoload
(define-derived-mode gmail-message-edit-server-mode text-mode "GMail/mirror"
  "Designed for GMail messages coming from google-chrome's \"Edit with Emacs\".

Not actually meant for editing. This just sets up the buffer as a
mirrored version of an html file that you'll be editing with the
actual `gmail-message-mode'.

This is supposed to be added to `edit-server-url-major-mode-alist',
so that it's called in an edit-server buffer. If you're trying to
use this in any other way, you're probably using the wrong
function. Try using (or extending) `gmail-message-mode' instead."
  :group 'gmail-message-mode
  (unless (and (boundp 'edit-server-edit-mode)
               edit-server-edit-mode)
    (error "This isn't an edit-server buffer!
You're probably using this mode wrong.
See the documentation for `gmail-message-edit-server-mode'."))
  (let ((file (gmm/-generate-temp-file-name))
        (save-function `(lambda ()
                          (with-current-buffer ,(current-buffer)
                            (edit-server-save)))))
    (setq gmm/-mirrored-file file)
    (write-file file :confirm)
    (add-hook 'edit-server-done-hook 'gmm/-reflect-temp-file nil :local)
    (find-file file)
    (add-hook 'ham-mode-md2html-hook save-function :append :local)))
(eval-after-load 'edit-server
  '(add-to-list 'edit-server-url-major-mode-alist
                '("mail\\.google\\.com" . gmail-message-edit-server-mode)))

(defun gmm/-generate-temp-file-name ()
  (let (file)
    (while (or (null file) (file-exists-p file))
      (setq file (format "%s%s-%s-%s.html" temporary-file-directory
                         (buffer-name) "gmm-mirror" (random 100000))))
    file))

(defun gmm/-reflect-temp-file ()
  "Make current buffer reflect file given by `gmm/-mirrored-file'"
  (erase-buffer)
  (insert-file-contents gmm/-mirrored-file))

(defvar gmm/-end-regexp
  "<br *clear=\"all\">\\|<div><div *class=\"gmail_extra\">\\|<div *class=\"gmail_extra\">"
  "Regexp defining where a message ends and signature or quote starts.")

(defcustom gmm/signature-properties
  `(display ,(if (char-displayable-p ?…) "..." "…")
            intangible t
            pointer arrow
            mouse-face mode-line-highlight
            keymap ,(let ((map (make-sparse-keymap)))
                      (define-key map [down-mouse-1] 'gmm/-expand-end)
                      (define-key map [remap self-insert-command] 'gmm/-expand-end)
                      (define-key map "\C-j" 'gmm/-expand-end)
                      (define-key map "\C-i" 'gmm/-expand-end)
                      (define-key map [return] 'gmm/-expand-end)
                      map))
  "Property list to use on the signature.

Does not affect the final e-mail. This is just used to hide
useless stuff from the user."
  :type '(repeat symbol (choice symbol string))
  :group 'gmail-message-mode
  :package-version '(gmail-message-mode . "1.0.1"))

(defun gmm/-expand-end ()
  "Expand the ending of the message, if it was collapsed."
  (interactive)
  (let ((inhibit-read-only t))
    (when (remove-text-properties
           (point-min) (point-max)
           gmm/signature-properties)
      (message "Signature and quotes expanded, see `%s' to disable hiding."
               'gmm/signature-properties))))

(defun gmm/-propertize-buffer ()
  "Add some text properties to the buffer, like coloring the signature."
  (goto-char (point-min))
  (when (search-forward-regexp gmm/-end-regexp nil :noerror)
    (add-text-properties (match-beginning 0) (point-max)
                         gmm/signature-properties)
    (message "Hiding garbage at the end. See `%s' to disable this"
             'gmm/signature-properties)))

(define-key gmail-message-mode-map (kbd "C-c C-z") 'gmm/save-finish-suspend)

;;;###autoload
(defun gmm/-set-amlist (&optional sym val)
  "Reset the auto-mode-alist."
  (when sym
    (set-default sym val))
  (mapc
   (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode)))
   gmm/auto-mode-list))
;;;###autoload
(mapc
 (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode)))
 gmm/auto-mode-list)

(provide 'gmail-message-mode)
;;; gmail-message-mode.el ends here.
