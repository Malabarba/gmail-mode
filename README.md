gmail-mode
==========

**gmail-mode** is an emacs major-mode for editing gmail messages using
markdown syntax, it is meant for use with browser plugins which allow
you to edit text fields with external applications (in this case,
emacs). See [Plugins][] for a list for each browser.

**The problem:** Lately, gmail messages have been using html. That
  made it very hard to edit them outside your browser, because you had
  to edit html source code (for instance, linebreaks were ignored and
  you had to type `<br>` instead).

**gmail-mode to the rescue:** Simply activate this mode in gmail
messages (See [Activation][]); the buffer is converted to markdown and
you may edit at will, but the file is still saved as html behind the
scenes so GMail won't know a thing! 

[Activation]: #Activation

[Plugins]: #Plugins




