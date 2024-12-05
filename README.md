# mxMarkEdit

Copyright Massimo Nardello, Modena (Italy) 2024.

mxMarkEdit is a free software for Mac for writing texts in [Markdown format](https://pandoc.org/MANUAL.html#pandocs-markdown) and easily exporting them to other formats with [Pandoc](https://pandoc.org). The software has been written in [Lazarus](https://www.lazarus-ide.org) and is released under the GPL version 3 license or following, available in www.gnu.org/licenses/gpl-3.0.html.

The software runs in English or Italian. In the “app” directory of the source code there is an app compiled for Mac with the Silicon chip (M1-M4) ready to be copied in the Application directory and run. The app is not notarized.

The software looks like this, with one possible color setting:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot1.png)

The main features of mxMarkEdit are the followings.

Some Markdown markers are hidden, as the text contained within them is properly formatted. These markers are:

* italics (\*), bold (\*\*) and bold italics (\*\*\*);
* code, in monospace font (`);
* lines of code, in monospace font (```);
* inline footnotes (contained between ^[ and ]);
* citations (start with the character > followed by a space).

The markers cannot be escaped as far as the formatting of mxMarkEdit is concerned.

At the left of the text, it’s available a list of the titles (starting with #) and todo items (starting with – [ ]  or – [X] ). By clicking on an item in this list, the cursor moves to the corresponding title or todo item. Moving the cursor in the text, the corresponding title or todo item is highlighted.

When a paragraph starts with a list heading (*, +, - or a number followed by a dot and a space), the “Return” key creates a new paragraph with the same heading. The numbered lists are properly renumbered if necessary.

The last four opened files are available in the “File” menu. When they are opened, the cursor move to the last position it had during the last editing.

The “Edit - Find” menu item allows to find the first occurrence – with the appropriate button or by pressing “Return” - or the following occurrences – with the appropriate button or by pressing “Command + Return” – of a sentence, or to replace all occurrences with another text. Search and replacement are not case sensitive. When using the replacement function, the \n code replaces the paragraph breaks, while the \t code replaces the tabs, both in the “Search text” field and in the “Replace with field”.

The “Edit - Disable formatting” menu item prevents the formatting of the text, improving the performance of the app with large documents.

The “Convert with Pandoc” menu item converts the current note into another format specified in the options of the software. The converted file is created in the same folder of the note.

The “Open another window” menu item opens another instance of the software if it’s located in the Applications directory.

The “Transparency” menu item activate or deactivate the transparency of the software interface.

The “Tools - Options” menu item opens the options, where are specified the name of the font of the text, the color of the footnotes and quotes, the color of the first level, second level and other level titles, the Pandoc options, the path to the Pandoc executable, the possible Word or Writer template to use as a reference and the format of the file the note should be converted to (.docx, .odt, etc.). See the Pandoc manual for more information.

The other menu items are self-explaining.

There are also some shortcuts beyond the ones related to the menu items:

* Meta + “+” or “-“: change the size of the normal font.
* Meta + Ctrl + “+” or “-“: change the size of the mono font, which is Menlo.
* Meta + Shift + Backspace: delete the current line.
* Meta + D: insert the current date.
* Meta + Shift + D: insert the current date and time.
* Meta + B: format the selected text in bold or the current word if no text is selected.
* Meta + I: format the selected text in bold or the current word if no text is selected.
* Meta + G: find the next recurrence of the text specified in the search form.
* Meta + T: create a todo item, or set it as done or to be done.
* Meta + Opt + Arrow up: move up the current paragraph.
* Meta + Opt + Arrow down: move down the current paragraph.
* Meta + Ctrl + Arrow up: select the previous heading.
* Meta + Ctrl + Arrow down: select the next heading.
* Meta + U: make uppercase the current word.
* Meta + Opt + Shift + U: make lowercase the current word.
* Meta + Opt + U: capitalise the current word.

The software creates the two configuration file:
- `/Users/[username]/Library/Preferences/mxmarkedit`
- `/Users/[username]/Library/Preferences/mxmarkedit.plist`
