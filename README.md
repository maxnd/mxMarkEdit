# mxMarkEdit

<img align=“left” src=“https://github.com/maxnd/mxMarkEdit/blob/main/icon128.png”>

Version 1.1.3, published on 3 January 2025.

Copyright Massimo Nardello, Modena (Italy) 2024 - 2025.

mxMarkEdit is a free software for Mac for writing texts and todo items in [Markdown format](https://pandoc.org/MANUAL.html#pandocs-markdown) and easily exporting them to other formats with [Pandoc](https://pandoc.org), that must be installed in the system in use. In each document, it’s available an Excel-like grid useful to manage various sets of data. The software has been written in [Lazarus](https://www.lazarus-ide.org) and is released under the GPL version 3 license or following, available in www.gnu.org/licenses/gpl-3.0.html. The software runs in English or Italian.

## Download and install

In the `app` directory of the source code there is an app compiled for Mac with the Silicon chip (M1-M4) ready to be copied in the `Application` directory and run. 

From version 1.1.2, **the app is notarized by Apple** (checked against harmful code), so it can run without any trouble also on recent macOS versions.

To download the app, click on this link: https://github.com/maxnd/mxMarkEdit/raw/main/app/mxMarkEdit.zip.

## Look of the app

The software looks like this, with one possible color setting:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot1.png)

When the tables grid is shown, the software looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot2.png)

The form that summarises the todo items typed in the document looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot3.png)

The form of the options of the app looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot4.png)

The main features of mxMarkEdit are the followings.

## Formatting

Some Markdown markers are hidden, as the text that follows them or is contained within them is properly formatted. These markers are:

* headers (beginning with `#`, `##`, `###`, `####`, `#####` and `######`);
* italics (`*` or `_`), bold (`**` or `__`) and bold italics (`***` or `___`);
* code, shown in monospace font (`);
* citations, which start with the character `>` followed by a space, that are shown with a smaller font size.

Other Markdown markers are not hidden, but are shown in a different color defined by the user:

* lines of code, shown in monospace font, among the markers \`\`\` at the beginning of a paragraph;
* footnotes, like `[^1]` and `[^1]: Text of the footnote`;
* inline footnotes, like `^[Text of the footnote.]`;
* links to websites or files, like `[my web site](www.mywebsite.com)`;
* links to pictures, like `![my picture](img.jpg)`.

The markers cannot be escaped as far as the formatting of mxMarkEdit is concerned. Pandoc should instead manage them properly.

When a paragraph starts with a list heading (`*`, `+`, `-` or a number followed by a dot and a space), the `Return` key creates a new paragraph with the same heading. The numbered lists are properly renumbered if necessary.

## Titles and todo list

At the left of the text, there’s a list of the titles (starting with `#`) and todo items (starting with `– [ ]`  or `– [X]`). By clicking on an item in this list, the cursor moves to the corresponding title or todo item. While moving the cursor in the text, the corresponding title or todo item is highlighted.

Above this list there is combo box in which it’s possible to select the level of the headings to be shown. The todo items and the heading under which the cursor is located are always shown.

In the todo items, it’s possibile to add a deadline. The format of the date should be `year-month-day`, where month and day are always of two digits. The date must be followed by 3 separator characters before the title of the todo item. For instance: `- [X] 2024-12-05 • Buy some food`. The shortcut `Meta + T` mentioned also below insert automatically the todo marker and the deadline in the proper format with a delay in days that is specified by the user in the options of the app (the default is 7 days).

## Tables

At the bottom of the text there’s an Excel-like grid of 105 columns and 2000 rows which may contain simple tables, which are horizontal portions of this grid. This is hidden by default, and can be shown dragging up with the mouse the splitter at the bottom of the text, or with the shortcut `Meta` + `Shift` + `T`. In the first column on the left, named `Tables names`, the user must type the title of a table (e.g. `Books`), and in the columns at its right (`A1`, `B1`, etc.) all the necessary fields (e.g. `Author`, `Title`, `Year`, etc.). The title is shown with the color of the `headings 2`, while the fields have the color of the `headings 3`. Below the titles, it’s possibile to insert any kind of data. It’s possibile to add more tables in the grid, adding other titles under the `Table names` column, like in the second screenshot above.

The content of the table is saved in a separate file with the same name of the one in use, but with the extension `.csv`. In this file the items are separated by tabs, and it may be easily imported in a spreadsheet. When the current document is exported to Pandoc, if there are some data in the tables grid, the software creates a new file with the extension `.export` which contains both the document and the tables properly formatted in Markdown format. In the converted document in Word or Writer format, the tables are located at the end of the text.

In the tables grid, it’s possible to select more cells dragging the mouse or holding the `Shift` key and pressing the arrow keys. The selected text can be copied in the clipboard and pasted in another position of the grid or in a document.

At the bottom, there’s the `Find` field useful to search for data contained in the grid. Type within it the text to be found and press `Return` to select the following cell after the current one in just the current column of the current table that contains that text. The search is case insensitive. To continue the search, select again the field and press `Return`, or use the shortcut `Meta + G` when the grid is focused.

See below for some useful shortcuts available in this grid.

## Menu items 

The last four opened files are available in the `File` menu. When they are opened, the cursor move to the last position it had during the last editing.

The `Edit - Find` menu item allows to find the first occurrence – with the button `First` or by pressing `Return` - or the following occurrences – with the button `Next` or by pressing `Command + Return` – of a sentence, or to replace all occurrences with another text. Search and replacement are not case sensitive. When using the replacement function, the `\n` code replaces the `Line feed`, which in macOS or Linux is the paragraph break, the `\r` code replaces the `Enter` while the `\t` code replaces the tabs, both in the `Search text` field and in the `Replace with` field. In document written on Windows systems, the paragraph breaks are made by `Line feed` + `Enter`.

The `Edit - Insert link to file` allows to select a file and to insert its path and name in the text preceded by the `file://` heading. All the possible spaces are converted with `%20`, so that the file name may be properly recognised as a link.

The `Edit - Find duplicate words` shows in red all the words of each paragraph, separated by `.`, `?` or `!` followed by `space` or `Return`, that are present twice in the same paragraph or in the previous one, to alert the user to use possibly a synonym; if there’s no selected text, all the document is checked after a confirmation. This may take some time.

The menu item `Edit - Show todo form` opens a form with the list of all the todo items. Pressing `Return` or double clicking on one of them selects the corresponding item in the document. Click on the headers of the columns of the form sorts the data, initially sorted by deadline. The date of the expired items is shown in red. The check box at the bottom allows to hide the todo items already done. The button `Toggle state` changes the state of the selected todo item from to be done to done, and vice versa. The button `Copy` copies into the clipboard the content of the form; the values are separated by tab, so that they may be pasted in the columns of a spreadsheet. See below for the shortcuts active on this form.

The `Edit - Show current title or toto` shows at the top of the left list the title or todo item the cursor is under to.

The `Edit - Disable formatting` menu item prevents the formatting of the text, improving the performance of the app with large documents.

The `Convert with Pandoc` menu item converts the current note into another format specified in the options of the software. The converted file is created in the same folder of the current document.

The `Open another window` menu item opens another instance of the software if it’s located in the `Applications` directory.

The `Transparency` menu item activate or deactivate the transparency of the software interface.

The `Tools - Options` menu item opens the options of the app, where are specified the name and the color of the font of the text, the name and the color of the font for code, the color of the links, of the footnotes, of the todo items, of the first level, second level and other level titles, the line spacing value (1.0 is the default), the delay in days for the new deadlines (7 is the default), the size of the normal font and of the code font, the Pandoc options, the path to the Pandoc executable, the possible Word or Writer template to use as a reference and the format of the file the note should be converted to (`.docx`, `.odt`, etc.). See the Pandoc manual for more information.

The other menu items are self-explaining.

## Shortcuts

There are also some shortcuts beyond the ones related to the menu items.

### In the main form

* `Meta` + `Z`: undo the last action.
* `Meta` + `Shift` + `Y`: redo the last action.
* `Meta` + `Opt` + `1-6`: select one of the six filter options above the title and todo list.
* `Meta` + `+` or `-`: change the size of the normal font.
* `Meta` + `Ctrl` + `+` or `-`: change the size of the mono font.
* `Meta` + `Shift` + `Backspace`: delete the current paragraph.
* `Meta` + `D`: insert the current date.
* `Meta` + `Shift` + `D`: insert the current date and time.
* `Meta` + `B`: format the selected text in bold or the current word if no text is selected.
* `Meta` + `I`: format the selected text in italics or the current word if no text is selected.
* `Meta` + `R`: if the cursor is inside a numbered list, renumber the list.
* `Meta` + `Shift` + `R`: renumber all the possible footnotes references.
* `Meta` + `G`: find the next recurrence of the text specified in the search form, even if it’s closed.
* `Meta` + `T`: create a todo item with a deadline whose delay in days is specified in the options (default is 7), or set it as done or to be done.
* `Meta` + `Opt` + `T`: create a todo item without a deadline, or set it as done or to be done.
* `Meta` + `Opt` + `Arrow up`: move up the current paragraph.
* `Meta` + `Opt` + `Arrow down`: move down the current paragraph.
* `Meta` + `Ctrl` + `Arrow up`: select the previous heading.
* `Meta` + `Ctrl` + `Arrow down`: select the next heading.
* `Meta` + `Shift` + `J`: set the current position of the cursor in the bookmark.
* `Meta` + `J`: move the cursor to the position already set in the bookmark.
* `Meta` + `E`: show the following paragraph with a green background, skipping the empty lines and the headings; this is useful to use mxMarkEdit for presentations.
* `Meta` + `U`: make uppercase the current word.
* `Meta` + `Opt` + `Shift` + `U`: make lowercase the current word.
* `Meta` + `Opt` + `U`: capitalise the current word.
* `Meta` + `Shift` + `T`: show the tables grid.
* `Meta` + `Shift` + `F`:
  * within a footnote reference in the document (e.g. `[^1]`), move the cursor to the corresponding footnote;
  * within a footnote (e.g. `[^1]: This is the text of the footnote.`), move the cursor to the corresponding footnote reference in the document; 
  * in other positions, create a new footnote reference and a new footnote, both properly numbered.

### In the tables grid

* `Meta` + `Shift` + `I`: insert a new row.
* `Backspace`: delete the content of the current cell.
* `Meta` + `Shift` + `Backspace`: delete the current row.
* `Meta` + `G`: search the text in the `Find` field starting from the current position and just in the current column.
* `Meta` + `Opt` + `Arrow up`: move up the current row.
* `Meta` + `Opt` + `Arrow down`: move down the current row.
* `Meta` + `Opt` + `Arrow left`: move left the current field just of the current table with its content (not the entire column of the grid).
* `Meta` + `Opt` + `Arrow right`: move right the current field just of the current table with its content (not the entire column of the grid).
* `Meta` + `Ctrl` + `Arrow up`: move the current table, with all its field, before the previous one.
* `Meta` + `Ctrl` + `Arrow down`: move the current table, with all its field, after the previous one.
* `Meta` + `Arrow up`: in the tables names column, select the previous table title, while in the other columns move to the top of the grid.
* `Meta` + `Arrow down`: in the tables names column, select the following table title, while in the other columns move to the last edited row of the grid.
* `Meta` + `Arrow left`: move to the left end of the grid.
* `Meta` + `Arrow right`: move to the right end of the grid.
* `Meta` + `Ctrl` + `S`: sort alphabetically the content of the current column in the current table.
* `Meta` + `C`: copy the content of the selected cells in the clipboard.
* `Meta` + `V`: paste the content of the clipboard in the current and following cells or in a document.
* `Esc`: undo the last changes.

### In the todo form

* `Meta` + `Opt` + `H`: hide the todo items already done.
* `Meta` + `T`: toggle the state of the selected todo item from to be done to done, and vice versa.

## Backup

When a file or two related files, if the tables grid has a content, that have been modified are closed, a backup copy is created with the `.bak` extension.

## Configuration files

The software creates these two configuration files that can be deleted to reset the configuration of the app:

- `/Users/[username]/Library/Preferences/mxmarkedit`
- `/Users/[username]/Library/Preferences/mxmarkedit.plist`

## Revision history

### Version 1.1.3

- Bugs fixing: when a new Markdown file was created, a grid file was created as well even if there were no data inside it.
