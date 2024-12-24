# mxMarkEdit

Version 1.0.8, published on 24 December 2024.

Copyright Massimo Nardello, Modena (Italy) 2024.

mxMarkEdit is a free software for Mac for writing texts and todo items in [Markdown format](https://pandoc.org/MANUAL.html#pandocs-markdown) and easily exporting them to other formats with [Pandoc](https://pandoc.org), that must be installed in the system in use. The software has been written in [Lazarus](https://www.lazarus-ide.org) and is released under the GPL version 3 license or following, available in www.gnu.org/licenses/gpl-3.0.html. The software runs in English or Italian.

## Download and install

In the `app` directory of the source code there is an app compiled for Mac with the Silicon chip (M1-M4) ready to be copied in the `Application` directory and run. The app is signed by the author, but it’s not notarized.  

To download the app, click on this link: https://github.com/maxnd/mxMarkEdit/raw/main/app/mxMarkEdit.zip.

To run the app on macOS Sequoia, follow the Apple guidelines in https://support.apple.com/en-us/102445. Basically, after you’ve tried to open the app, follow these steps:

1. Open `System Settings`.
2. Click `Privacy & Security`, scroll down, and click the `Open Anyway` button to confirm your intent to open the app.
3. The warning prompt reappears and you can click `Open`.

## Look of the app

The software looks like this, with one possible color setting:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot1.png)

This is the form that summarises the todo items typed in the document:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot2.png)

## Main features

The main features of mxMarkEdit are the followings.

Some Markdown markers are hidden, as the text that follows them or is contained within them is properly formatted. These markers are:

* headers (beginning with `#`, `##`, `###`, `####`, `#####` and `######`);
* italics (`*` or `_`), bold (`**` or `__`) and bold italics (`***` or `___`);
* code, printed in monospace font (`);
* citations, which start with the character `>` followed by a space, that are printed with a smaller font size.

Other Markdown markers are not hidden, but are printed in a different color defined by the user:

* lines of code, printed in monospace font (\`\`\`);
* inline footnotes, contained between `^[` and `]`);
* links to websites or files, like `[my web site](www.mywebsite.com)`;
* links to pictures, like `![my picture](img.jpg)`.

The markers cannot be escaped as far as the formatting of mxMarkEdit is concerned. Pandoc should instead manage them properly.

At the left of the text, there’s a list of the titles (starting with `#`) and todo items (starting with `– [ ]`  or `– [X]`). By clicking on an item in this list, the cursor moves to the corresponding title or todo item. Moving the cursor in the text, the corresponding title or todo item is highlighted.

Above this list there is combo box in which it’s possible to select the level of the headings to be shown. The todo items and the heading under which the cursor is located are always shown.

In the todo items, it’s possibile to add a deadline. The format of the date should be `year-month-day`, where month and day are always of two digits. The date must be followed by 3 separator characters before the title of the todo item. For instance: `- [X] 2024-12-05 • Buy some food`. The shortcut `Meta + T` mentioned below insert automatically the todo marker and the deadline a week ahead in the proper format.

When a paragraph starts with a list heading (`*`, `+`, `-` or a number followed by a dot and a space), the `Return` key creates a new paragraph with the same heading. The numbered lists are properly renumbered if necessary.

When a file is closed, a backup copy is created.

## Menu items 

The last four opened files are available in the `File` menu. When they are opened, the cursor move to the last position it had during the last editing.

The `Edit - Find` menu item allows to find the first occurrence – with the appropriate button or by pressing `Return` - or the following occurrences – with the appropriate button or by pressing `Command + Return` – of a sentence, or to replace all occurrences with another text. Search and replacement are not case sensitive. When using the replacement function, the `\n` code replaces the paragraph breaks, while the `\t` code replaces the tabs, both in the `Search text` field and in the `Replace with field`.

The `Edit - Insert link to file` allows to select a file and to insert its path and name in the text preceded by the `file://` heading. All the possible spaces are converted with `%20`, so that the file name may be properly recognised as a link.

The `Edit - Find duplicate words` prints in red all the words of each paragraph, separated by dot and space or dot and `Return`, that are present also in the previous paragraph, to alert the user to use possibly a synonym; if there’s no selected text, all the document is checked after a confirmation. This may take some time.

The menu item `Edit - Show todo form` opens a form with the list of all the todo items. Pressing `Return` or double clicking on one of them selects the corresponding item in the document. Click on the headers of the columns of the form to sort the data, which are initially sorted by deadline. The date of the expired items is printed in red. The box at the bottom allows to hide the todo items already done.

The `Edit - Show current title or toto` put at the top of the left list the title or todo item the cursor is under to.
	
The `Edit - Disable formatting` menu item prevents the formatting of the text, improving the performance of the app with large documents.

The `Convert with Pandoc` menu item converts the current note into another format specified in the options of the software. The converted file is created in the same folder of the note.

The `Open another window` menu item opens another instance of the software if it’s located in the `Applications` directory.

The `Transparency` menu item activate or deactivate the transparency of the software interface.

The `Tools - Options` menu item opens the options, where are specified the name and the color of the font of the text, the name and the color of the font for code, the color of the links, of the footnotes, of the todo items, of the first level, second level and other level titles, the delay in days for the new deadlines, the Pandoc options, the path to the Pandoc executable, the possible Word or Writer template to use as a reference and the format of the file the note should be converted to (`.docx`, `.odt`, etc.). See the Pandoc manual for more information.

The other menu items are self-explaining.

## Shortcuts

There are also some shortcuts beyond the ones related to the menu items:

* `Meta` + `Opt` + `1-6`: select one of the six filter options above the title and todo list.
* `Meta` + `+` or `-`: change the size of the normal font.
* `Meta` + `Ctrl` + `+` or `-`: change the size of the mono font.
* `Meta` + `Shift` + `Backspace`: delete the current line.
* `Meta` + `D`: insert the current date.
* `Meta` + `Shift` + `D`: insert the current date and time.
* `Meta` + `B`: format the selected text in bold or the current word if no text is selected.
* `Meta` + `I`: format the selected text in italics or the current word if no text is selected.
* `Meta` + `G`: find the next recurrence of the text specified in the search form, even if it’s closed.
* `Meta` + `T`: create a todo item with a deadline whose delay in days is specified in the options (default is 7), or set it as done or to be done.
* `Meta` + `Opt` + `T`: create a todo item without a deadline, or set it as done or to be done.
* `Meta` + `Opt` + `Arrow up`: move up the current paragraph.
* `Meta` + `Opt` + `Arrow down`: move down the current paragraph.
* `Meta` + `Ctrl` + `Arrow up`: select the previous heading.
* `Meta` + `Ctrl` + `Arrow down`: select the next heading.
* `Meta` + `Shift` + `J`: set the current position of the cursor in the bookmark.
* `Meta` + `J`: move the cursor to the position already set in the bookmark.
* `Meta` + `E`: prints the following paragraph with a green background, skipping the empty lines and the headings; this is useful to use mxMarkEdit for presentations.
* `Meta` + `U`: make uppercase the current word.
* `Meta` + `Opt` + `Shift` + `U`: make lowercase the current word.
* `Meta` + `Opt` + `U`: capitalise the current word.

## Configuration files

The software creates these two configuration files that can be deleted to reset the configuration of the app:

- `/Users/[username]/Library/Preferences/mxmarkedit`
- `/Users/[username]/Library/Preferences/mxmarkedit.plist`
