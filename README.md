gmail-message-mode
==========

**gmail-message-mode** is an emacs major-mode for editing gmail
messages using markdown syntax, it is meant for use with browser
plugins which allow you to edit text fields with external applications
(in this case, emacs). See [Plugins][] below for a list for each
browser.

**The problem:** Lately, gmail messages have been demanding html. That
  made it very hard to edit them outside your browser, because you had
  to edit html source code (for instance, linebreaks were ignored and
  you had to type `<br>` instead).
  
**gmail-message-mode to the rescue:** Simply activate this mode in
  gmail messages (See [Activation][]); the buffer is converted to
  markdown and you may edit at will, but the file is still saved as
  html behind the scenes so GMail won't know a thing! *See
  [ham-mode][1] to understand how this works.*


The point is that you can write gmail messages as *plain text*, and
they will show up fine when you go back to gmail. And if you want more
than plain text, you can write in Markdown syntax, which provides you
with an easy way to add *bullet-points*, *lists*, *bold*, *italics*,
etc, (which is, in fact, faster than mousing through gmail's web
interface).

Activation
----------
Make sure you install it:

    M-x package-install RET gmail-message-mode
    
And that's it!  
*(if you install manually, note that it depends on [ham-mode][1])*

This package will (using `auto-mode-alist`) configure emacs to
activate `gmail-message-mode` whenever you're editing a file that
seems to be a gmail message (if you don't want this, see [Disabling][]
below). However, given the wide range of possible plugins, it's hard
to catch them all. You may have to add entries manually to
`auto-mode-alist`, to make sure `gmail-message-mode` is activated.


## Plugins ##

1. **Firefox** - [It's all text][] combined with [Old Compose][] (see [this thread][] on why you need the second).
2. **Google-Chrome (or Chromium)** - [Edit with emacs][]
3. **Conkeror** - [Spawn Helper (built-in)][]
4. *Others* - Tried it in another browser? [let me know][]!

## Disabling ##

To keep `gmail-message-mode` from automatically adding itself to your
`auto-mode-alist`, just add the following snippet before the package
is loaded:

    (setq 


[Disabling]: #disabling

[Activation]: #activation

[Plugins]: #plugins

[It's all text]: https://addons.mozilla.org/en-US/firefox/addon/its-all-text/

[Edit with emacs]: http://www.emacswiki.org/emacs/Edit_with_Emacs

[Spawn Helper (built-in)]: http://conkeror.org/ConkerorSpawnHelper

[this thread]: http://github.com/docwhat/itsalltext

[Old Compose]: http://oldcompose.com/

[1]: https://github.com/Bruce-Connor/ham-mode

[let me know]: https://github.com/Bruce-Connor/gmail-message-mode/issues/new

