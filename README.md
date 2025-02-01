<img align=“left” src=“https://github.com/maxnd/mxMarkEdit/blob/main/icon128.png”   width=“64”>

# mxMarkEdit

Version 1.2.17, published on February 1 2025.

Author and copyright: Massimo Nardello, Modena (Italy) 2024 - 2025.

[Read this manual in PDF](https://github.com/maxnd/mxMarkEdit/raw/main/manuals/mxmarkedit-user-manual-en.pdf).

[Read the Italian translation of this manual in PDF](https://github.com/maxnd/mxMarkEdit/raw/main/manuals/mxmarkedit-user-manual-it.pdf).

The PDF manual may be downloaded also within the app, with the `Help - Download manual` menu item.

---

To watch some English video about the functionalities of mxMarkEdit, see https://www.youtube.com/@MassimoNardello. The conversations have been generated by Google NotebookLM in January 2025 just from this `Readme.md` file, while the pictures are just the screenshots of the app.

😀 You use mxMarkEdit, but you feel that it has far too many shortcuts and functionalities that definitely make it not for humans? Download [this nice wallpaper](https://github.com/maxnd/mxMarkEdit/raw/main/wallpaper/wallpaper-gray-mxmarkedit.png) to show everybody why!

---

mxMarkEdit is a free software for Mac for writing texts and todo items in [Markdown format](https://pandoc.org/MANUAL.html#pandocs-markdown) and easily exporting them to other formats with [Pandoc](https://pandoc.org), provided that it’s installed in the system in use. In each document, it’s available an Excel-like grid useful to manage various sets of data, and also a file manager to search for a sentence in all the documents or spreadsheets contained in a folder. It's also possible to use the app as a presentation manager.

The software has been written in [Lazarus](https://www.lazarus-ide.org) and is released under the GPL version 3 license or following, available in www.gnu.org/licenses/gpl-3.0.html. The software runs in English or Italian.

mxMarkEdit *doesn't use Electron* because of its performance and [security issues](https://medium.com/@flynn.kelseyelizabeth/dont-use-electron-until-you-ve-read-this-article-52d30401303d). The formatting of Markdown text is made by an original algorithm written in Free Pascal.

mxMarkEdit has been deeply influenced by [Org-mode](https://orgmode.org). Although it has far less functionalities and customisations than the latter, it’s a standard Mac app which benefits of the Apple spell checking and standard shortcuts.

### Privacy

mxMarkEdit does not collect any user data and does not access to his or her pictures, files or any other personal data. The data created with the app are stored only on the computer of the user and possibly on his or her cloud.


## Download and install

In the `app` directory of the source code there is an app compiled for Mac with the Silicon chip (M1 or following) ready to be copied in the `Application` directory and run. 

*The app is notarized by Apple* (checked against harmful code), so it can run without any trouble also on recent macOS versions.

To download the app, click on this link: https://github.com/maxnd/mxMarkEdit/raw/main/app/mxMarkEdit.zip.

To install Pandoc, not necessary to use the app but necessary to export the Markdown files in other formats, click on this link: https://pandoc.org/installing.html.

To open automatically Markdown files with mxMarkEdit, follow the [Apple Guidelines](https://support.apple.com/en-sa/guide/mac-help/mh35597/mac).

## Look of the app

The software looks like this, with one possible color setting:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot1.png)

When the tables grid is shown, the software looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot2.png)

A normal presentation in Markdown format looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot5.png)

The same presentation optimised with `Tools - Optimise presentation...` and shown in presentation mode looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot6.png)

The form that summarises the todo items typed in the document looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot3.png)

The form of the options of the app looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot4.png)

## Formatting

Some Markdown markers are hidden, as the text that follows them or is contained within them is properly formatted. These markers are:

* headers (beginning with `#`, `##`, `###`, `####`, `#####` and `######`);
* italics (`*` or `_`), bold (`**` or `__`) and bold italics (`***` or `___`);
* code, shown in monospace font (`` ` ``);
* citations, which start with the character `>` followed by a space, that are shown with a lighter or darker background, according to the mode in use.

Other Markdown markers are not hidden, but are shown in a different color defined by the user:

* lines of code, shown in monospace font, among the markers `` ``` `` at the beginning of a paragraph;
* footnotes, like `[^1]` and `[^1]: Text of the footnote`;
* inline footnotes, like `^[Text of the footnote.]`;
* links to websites or files, like `[my web site](www.mywebsite.com)`;
* links to pictures, like `![my picture](img.jpg)`.

The markers cannot be escaped as far as the formatting of mxMarkEdit is concerned. Pandoc should instead manage them properly.

To have a Markdown document properly exported in other formats with Pandoc, the paragraphs must be separated by an empty line, except in the lists, numbered or not.

When a paragraph starts with a list heading (`*`, `+`, `-` or a number followed by a dot and a space), the `Return` key creates a new paragraph with the same heading. The numbered lists are properly renumbered if necessary.

To move a heading and all its relative content, included possible headings of lower level (that is, having more `#` at the beginning), in another position of the text, move the cursor within this heading without doing any selection and cut all in the clipboard with the shortcut `Meta` + `Shift` + `X`. The text in the clipboard can then be pasted elsewhere.

Links are properly recognised and formatted if the formatting is not disabled. Anyway, if they contain some Markdown markers, like `*` or `_`, these markers are hidden, although they are still present and the link works normally when clicked upon. To show the link with all its characters, even possible markers, format it as a line of code including it among two `` ` ``. Anyway, when the link is formatted as a proper Markdown link, like `[my web site](www.mywebsite.com)`, the possible Markdown markers are properly shown.

The formatting should work instantly, without delaying the typing, at least up to about 250,000 characters (test made in a Mac Pro with M1 chip and 8 GB of RAM). To work speedily with bigger documents, disable the formatting and, if necessary, also the titles and toto list (see below the `Edit` menu notes). In this way, even a text of few millions of characters may be modified without any delay in typing.

By default, the app deactivate text formatting for documents bigger than 250,000 characters. This value can be modified in the options of the app (see below) to fit the performance of one's own Mac. The size of the document is calculated by the size of the file, not counting the characters, so it could be a bit inaccurate.

## Titles and todo list

At the left of the text, there’s a list of the titles (starting with `#`) and todo items (starting with `– [ ]`  or `– [X]`). By clicking on an item in this list, the cursor moves to the corresponding title or todo item. While moving the cursor in the text, the corresponding title or todo item is highlighted.

Above this list there is combo box in which it’s possible to select the level of the headings to be shown. The todo items and the heading under which the cursor is located are always shown.

In the todo items, it’s possible to add a deadline. The format of the date should be `year-month-day`, where month and day are always of two digits. The date must be followed by 3 separator characters before the title of the todo item. For instance: `- [X] 2024-12-05 • Buy some food`. The shortcut `Meta + T` mentioned also below insert automatically the todo marker and the deadline in the proper format with a delay in days that is specified by the user in the options of the app (the default is 7 days).

## Tables

At the bottom of the text there’s an Excel-like grid of 105 columns and 10000 rows which may contain simple tables, which are horizontal portions of this grid. This is hidden by default, and can be shown dragging up with the mouse the splitter at the bottom of the text, or with the shortcut `Meta` + `Shift` + `T`. In the first column on the left, named `Tables names`, the user must type the title of a table (e.g. `Books`), and in the columns at its right (`A1`, `B1`, etc.) all the necessary fields (e.g. `Author`, `Title`, `Year`, etc.). The title is shown with the color of the `headings 2`, while the fields have the color of the `headings 3`. Below the fields, it’s possible to insert any kind of data. It’s possible to add more tables in the grid, adding other titles under the `Table names` column followed by some fields, like in the second screenshot above.

If it’s necessary to insert dates in a field, use the format `year-month-day`, where month and day are always of two digits (e.g. `2025-01-05`), so that the sorting on this field will produce a correct result.

The content of the grid is saved in a separate file with the same name of the one in use, but with the `.csv` extension. In this file the items are separated by tabs, and it may be easily imported in a spreadsheet. When the current document is exported to Pandoc, if there are some data in the grid, the software creates a new file with the extension `.export` which contains both the document and the tables properly formatted in Markdown format. In the converted document in Word or Writer format, the tables are located at the end of the text.

When a `.cvs` file is loaded, the grid is not automatically shown, but in the status bar at the bottom, after the name of the `.md` file, it’s shown the label `& .csv`, to alert the user that there are some data inside the grid.

In the grid, it’s possible to select more cells dragging the mouse or holding the `Shift` key and pressing the arrow keys. The selected text can be copied in the clipboard and pasted in another position of the grid or in a document, or deleted. It’s possible also to paste some data copied from a spreadsheet like Excel or Numbers. See below for the shortcuts useful to do that.

At the bottom, there’s the `Find` field useful to search for data contained in the grid. Type within it the text to be found and press `Return` or the shortcut `Meta + G` to select the following cell after the current one in just the current field (column) of the current table that contains that text. Anyway, if the selected column is the first, containing the table names, the search will look for the following table names. The search is case insensitive. To continue the search, select again the field and press `Return`, or use the shortcut `Meta + G` when the grid is focused.

If a cell contains a formula, in the cell below it will be reported the result of the calculation of all the numbers contained in the previous cells of the same field (column) and of the same table. The result will be updated when a value in those cells will be changed. Textual contents and improperly formatted numbers will not be considered. The name of the table and of the field (column) must be specified, otherwise no result will be shown.

The formulas are the followings:

- `------` or `---sum`: reports the sum of the numbers;
- `---max`: reports the bigger number;
- `---min`: reports the smaller number;
- `---avg`: reports the average of the numbers;
- `---count`: reports the total number of the numbers;

The formulas are shown in the color set by the user for the code (see below the option of the app for more information).

It's possible to sort the content of the current column of the current table with the shortcut `Meta` + `Ctrl` + `S` (ascending) or `Meta` + `Ctrl` + `Shift` + `S` (descending). Numeric content are put before textual ones, and the possible empty lines before numbers. If a formula is present in any column, the sorting is limited to the rows before it. If no formula is present and the table is not followed by another table, with a name in the tables names column, it's necessary to mark the bottom row of the current table creating a fictional table after it; it's enough to write just its title.

To delete the content of the grid so that it’s not loaded any more, delete the related file with the `.csv` extension when it’s not loaded.

See below for some useful shortcuts available in this grid.

## Presentations

it’s possible to use the app for presentations:

1. load a document, possibly with some tables, containing the sequential text of the slides in Markdown format, with many paragraphs under titles and subtitles, like a normal text;
2. enlarge possibly the font size of the document so that it can be seen easily by the participants;
3. share the interface of mxMarkEdit with a video projector or on a video call with apps like Webex, FaceTime or Zoom;
4. begin the presentation and use the shortcut `Meta` + `E` to show the paragraph following the current one, or `Meta` + `Shift` + `E` to show the paragraph previous the current one, both in black or white, according to the theme in use, skipping the empty lines, and to fade the other text; when the presentation mode is enabled with one of these two shortcuts, the following or previous paragraphs may be selected also with the shortcuts `Meta` + `Arrow down` and `Meta` + `Arrow up`;
5. comment the content of the paragraph in black or white; the participants see the text that is currently focused, and its position in the structure of the document in the left bar, which anyway can be hidden;
6. if the reference to some tables is needed, show the grid, locate the table and comment its data with the participants using possibly the zoom functionalities of Mac to make visible the small numbers.

In presentation mode, the status bar is hidden, and the spell checking is automatically disabled. To quit the presentation mode, press `Esc` or click anywhere in the text. Possibly, reenable the spell checking.

Keep the presentation in standard Markdown format, to be able to export it with Pandoc without any trouble, and then create a copy of it optimised for mxMarkEdit with the menu item `Tools` - `Optimise presentation...` if it has to be presented with mxMarkEdit.

## Files management

The `File - Search in files...` menu item open a form to search a word or a sentence within all the `.md` (document) and `.csv` (tables) files contained in a folder specified by the user, including its possible subfolders. Specify the folder to search in with the button `Set folder`, whose value will be remembered by the app, and then type the text to search for in the field `Find`. Then press `Return` or click on the button `Find`. 

In the grid above the buttons it will be shown a row for each occurrence of the text to be found both in the documents and in the tables files, along with the context in which it appears (some words before and after it). The columns of the grid - path, file name and context - can be sorted with a click on their headers. Pressing `Return` on a row of the grid, or double clicking on it, make the app open the corresponding file, and move the cursor to the position of the text to be found, or, if the file is a `.csv` one, to the cell that contains it. 

The search in files returns maximum 100 recurrences for each document or spreadsheet analysed, and stops at 5,000 global recurrences, asking the user to possibly refine it. Also emojis can be searched for.

Moving the mouse over the label of the file path and name, at the bottom left of the main interface of the app, a hint appears with the complete content, even if in the label it's truncated due to its length.

## Menu items

The last four opened files are available in the `File` menu. When they are opened, the cursor move to the last position it had during the last editing, and the width of the columns of the grid is remembered as well by the app.

The `Edit - Find` menu item allows to find the first occurrence – with the button `First` or by pressing `Return` - or the following occurrences – with the button `Next` or by pressing `Command + Return` – of a sentence, or the previous ones with the button `Previous`, or to replace all occurrences with another text. Search and replacement are not case sensitive. See below for some useful shortcuts to use these functionalities. When using the replacement function, the `\n` code replaces the `Line feed`, which in macOS or Linux is the paragraph break, the `\r` code replaces the `Enter` while the `\t` code replaces the tabs, both in the `Search text` field and in the `Replace with` field. In document written on Windows systems, the paragraph breaks are made by `Line feed` + `Enter`. Finally, when looking for the next recurrence of an emoji, it might be necessary to move the cursor after the previous finding to find the next one.

The `Edit - Insert link to file` menu item allows to select one or more files and to insert their path and name in the text within the Markdown markers and preceded by the `file://` heading. All the possible spaces are converted with `%20`, so that the file name may be properly recognised as a link. It’s not possible to insert relative links due to macOS restrictions.

The `Edit - Show current title or toto` menu item shows at the top of the left list, after five other items if existing, the title or todo item the cursor is under to.

The `Edit - Show todo form` menu item opens a form with the list of all the todo items. Pressing `Return` or double clicking on one of them selects the corresponding item in the document. Click on the headers of the columns of the form sorts the data, initially sorted by deadline. The date of the expired items is shown in red. The check box at the bottom allows to hide the todo items already done. The button `Toggle state` changes the state of the selected todo item from to be done to done, and vice versa. The button `Copy` copies into the clipboard the content of the form; the values are separated by tab, so that they may be pasted in the grid or in a spreadsheet. See below for the shortcuts active on this form.

The `Edit - Show duplicate words` menu item shows in red all the words of each sentence that are present twice in the same sentence or in the previous one, to alert the user to use possibly a synonym. Moving the cursor or clicking somewhere in the text remove the red color.

The `Show words recurrence form` menu item opens a form with the list of all the words used in the current document, excluded the possible YAML headings (`title:`, `author:` etc.), and their recurrence, that is how many times they appear in the document. By default, the list is sorted by recurrence, but it’s possible to click on the radio item `Sort by words`, at the right bottom, to sort the list by name. Below the grid there is a field in which it’s possible to type the words that should not be considered, like articles, separated by commas. These words are automatically sorted. Below this field, there are some buttons to include the word selected in the grid within the words to skip, to update the results after having changed the words to skip, to copy in the clipboard the content of the grid and to close the form.

The `Edit - Disable formatting` menu item prevents the formatting of the text, except the normal font name, color and size and the line spacing, greatly improving the performance of the app with very large documents. In the options, it's possible to set the number of characters of a document beyond which it's loaded without formatting, just activating automatically this functionality.

The `Tools - Optimise presentation...` menu item creates a copy of the current presentation optimised to be shown with mxMarkEdit, and saves it with the name of the file in use plus ` - mxMarkEdit` addition. Basically, the various items of first level are separated by an empty paragraph, and the repeated titles of the slides are removed.

The `Tools - Convert with Pandoc` menu item

The `Tools - Open another window` menu item opens another instance of the software if the app is located in the `Applications` directory.

The `Tools - Transparency` menu item activate or deactivate two levels of transparency of the interface of the software, giving the impression to watch or type on a dark glass. This functionality is just for aesthetic reasons.

The `Tools - Options` menu item opens the options of the app, where are specified 

- the name and the color of the font of the text;
- the name and the color of the font for code;
- the color of the links (just the name, the linked file has the code font color);
- the color of the footnotes;
- the color of the todo items;
- the color of the first level titles; 
- the color of the second level titles;
- the color of the other levels titles;
- the size of the normal font;
- the size of the code font;
- the line spacing value (1.0 is the default);
- the delay in days for the new deadlines (7 is the default);
- the number of characters of a document beyond which it's loaded without formatting; it's calculated by the size of the file, not counting the characters, so it could be a bit inaccurate;
- the Pandoc options;
- the path of the Pandoc executable;
- the possible template (a file of Word, PowerPoint, Writer, etc.) to use as a reference;
- the format of the file the note should be converted to (`.docx`, `.odt`, etc.). 

A green or red circle is shown at the right of the fields of the path of the Pandoc executable and of the template to inform that the file are present or not. See the Pandoc manual for more information.

The `Help` - `Shortcuts list` menu item show a list of all the shortcuts available that are not related to menu items.

The other menu items are self-explaining.

## Shortcuts

There are also some shortcuts beyond the ones related to the menu items.

### In the main form

* `Meta` + `Z`: undo the last action in the document (not in the grid).
* `Meta` + `Shift` + `Y`: redo the last action in the document (not in the grid).
* `Meta` + `Opt` + `1-6`: select one of the six filter options above the title and todo list.
* `Meta` + `+` or `-`: change the size of the normal font.
* `Meta` + `Ctrl` + `+` or `-`: change the size of the mono font, use for code.
* `Meta` + `Shift` + `Backspace`: delete the current paragraph.
* `Meta` + `D`: insert the current date.
* `Meta` + `Shift` + `D`: insert the current date and time.
* `Meta` + `B`: format the selected text in bold, or the current word if no text is selected.
* `Meta` + `I`: format the selected text in italics, or the current word if no text is selected.
* `Meta` + `R`: if the cursor is inside a numbered list, renumber the list.
* `Meta` + `Shift` + `R`: renumber all the possible footnotes references if they have been manually changed.
* `Meta` + `G`: find in the document the next recurrence of the text specified in the search form, even if it’s closed.
* `Meta` + `Shift` + `G`: find in the document the previous recurrence of the text specified in the search form, even if it’s closed.
* `Meta` + `T`: create a todo item with a deadline whose delay in days is specified in the options (default is 7), or set it as done or to be done.
* `Meta` + `Opt` + `T`: create a todo item without a deadline, or set it as done or to be done.
* `Meta` + `Opt` + `Arrow up`: move up the current paragraph.
* `Meta` + `Opt` + `Arrow down`: move down the current paragraph.
* `Meta` + `Ctrl` + `Arrow up`: select the previous heading.
* `Meta` + `Ctrl` + `Arrow down`: select the next heading.
* `Meta` + `Opt` + `.`: transform a list with dashes (`-`) into a numbered list.
* `Meta` + `Ctrl` + `.`: transform a numbered list into a with dashes (`-`).
* `Meta` + `Shift` + `J`: set the current position of the cursor in the bookmark.
* `Meta` + `J`: move the cursor to the position already set in the bookmark.
* `Meta` + `E`: show the following paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Shift` + `E`: show the previous paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Arrow down`: if presentation mode has been enabled with `Meta` + `E` or `Meta` + `Shift` + `E`, show the following paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Arrow up`: if presentation mode has been enabled with `Meta` + `E` or `Meta` + `Shift` + `E`, show the previous paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `U`: make uppercase the current word.
* `Meta` + `Opt` + `Shift` + `U`: make lowercase the current word.
* `Meta` + `Opt` + `U`: capitalize the current word.
* `Meta` + `Shift` + `T`: show the tables grid.
* `Meta` + `Shift` + `X`: if the cursor is within a heading, cut in the clipboard the same heading and all the text that is under it, included possible headings of lower levels; this text may be pasted elsewhere;
* `Meta` + `Shift` + `F`:
  * within a footnote reference in the document (e.g. `[^1]`), move the cursor to the corresponding footnote;
  * within a footnote (e.g. `[^1]: This is the text of the footnote.`), move the cursor to the corresponding footnote reference in the document; 
  * in other positions, create a new footnote reference and a new footnote, both properly automatically numbered.

### In the tables grid

* `Meta` + `Shift` + `I`: insert a new row.
* `Backspace`: delete the content of the selected cell or cells, after confirmation.
* `Meta` + `Shift` + `Backspace`: delete the current row, after confirmation if it’s not empty.
* `Meta` + `G`: search the text typed in the `Find` field starting from the current position and just in the current column (field) of the current table.
* `Meta` + `Opt` + `Arrow up`: move up the current row.
* `Meta` + `Opt` + `Arrow down`: move down the current row.
* `Meta` + `Opt` + `Shift` + `I`: add a new column in the current table, after confirmation.
* `Meta` + `Opt` + `Shift` + `Backspace`: delete the current column in the current table, after confirmation, if it has a title and if it’s not the first one, containing the tables names.
* `Meta` + `Opt` + `Arrow left`: move left the current Column (field) just of the current table (not the entire column of the grid) with its content.
* `Meta` + `Opt` + `Arrow right`: move right the current column (field) just of the current table (not the entire column of the grid) with its content.
* `Meta` + `Ctrl` + `Arrow up`: move the current table, with all its field, before the previous one.
* `Meta` + `Ctrl` + `Arrow down`: move the current table, with all its field, after the following one.
* `Meta` + `Arrow up`: in the tables names column, select the previous table title, while in the other columns move to the top of the grid.
* `Meta` + `Arrow down`: in the tables names column, select the following table title, while in the other columns move to the last edited row of the current column.
* `Meta` + `Arrow left`: move to the first column of the grid.
* `Meta` + `Arrow right`: move to the last right edited column of the current row.
* `Meta` + `Ctrl` + `S`: sort ascending the content of the current column (field) in the current table, after confirmation; see `Tables` title above for more information.
* `Meta` + `Ctrl` + `Shift` + `S`: sort descending the content of the current column (field) in the current table, after confirmation; see `Tables` title above for more information.
* `Meta` + `Ctrl` + `F`: move the cursor in the search field, if it’s visible.
* `Meta` + `C`: copy the content of the selected cells in the clipboard.
* `Meta` + `V`: paste the content of the clipboard in the current (and following) cells.
* `Esc`: undo the last changes while the editor of a cell in the grid is still active.

### In the todo form

* `Meta` + `Opt` + `H`: hide the todo items already done.
* `Meta` + `T`: toggle the state of the selected todo item from to be done to done, and vice versa.
* `Esc`: close the form.

### In the search files form

* `Meta` + `Shift` + `F`: move the cursor in the `Find` field.
* `Esc`: close the form.

### In the words recurrence form

* `Esc`: close the form.

## Backup

When a `.md` file that has been modified is closed, a backup copy is created with the `.bak` extension. The same happens for the `.cvs` file, with the `.cvs.bak` extension.

## Configuration files

The software creates these two configuration files that can be deleted to reset the configuration of the app, included the colours:

- `/Users/[username]/Library/Preferences/mxmarkedit`
- `/Users/[username]/Library/Preferences/mxmarkedit.plist`

## Mentions

- Mentioned in [Indie Apps Catalog](https://indieappcatalog.com/app/991483088552/mxmarkedit).

## Revision history

#### Version 1.2.17

- Added menu item `File - Insert...`, to insert the content of a Markdown file in the current one at the cursor position.
- Deleting an empty row in the grid now doesn't require confirmation.
- Minor graphic improvements in presentation mode.
- Small bugs fixing.

#### Version 1.2.16

- Bugs fixing: within links, some Markdown markers were hidden and still active in formatting the text.

#### Version 1.2.15

- Bugs fixing: in the grid below the editor, the text could appear shadowed.

#### Version 1.2.14

- The menu `?` has been renamed as `Help`.
- Added the possibility to download the manual (see menu item `Help - Download manual`).
- Small bugs fixing.

#### Version 1.2.13

- In presentation mode, also the list of titles and todos is faded, and spell checking is automatically disabled.
- Minor graphic improvements.
- Small bugs fixing.

#### Version 1.2.12

- The links recognition is now suspended when the formatting is disabled, greatly improving the performance of the app with very large files.
- Small bugs fixing.

#### Version 1.2.11

- Added the menu item `Tools` - `Optimise presentation` (see above).
- Improved the color of the links: now the linked file has the color of the code font.

#### Version 1.2.10

- In the presentation mode, the text is faded and the selected paragraph is white or black, according to the theme in use, without any highlighting.
- Added new shortcut: `Meta` + `Shift` + `E` or `Meta` + `Arrow up` to select the previous paragraph during presentation.
- Added new shortcut: `Meta` + `Arrow down` to select the next paragraph during presentation.

#### Version 1.2.9

- The `Edit - Show current title or toto` menu item shows the title or todo item the cursor is under to at the top of the left list, after five other items if existing.
- In the presentation mode, the status bar is hidden.
- Bugs fixing: the find next function could crash the app.

#### Version 1.2.8

- The search in files now returns maximum 100 recurrences for each document or spreadsheet analysed, and stops at 5,000 global recurrences.
- Better arrangement of some menu items.
- Minor graphics improvements.

#### Version 1.2.7

- Added the shortcut `Meta` + `Ctrl` + `Shift` + `S` to sort descending the content of the current column in the current table.

#### Version 1.2.6

- In the options, has been added the specification of the number of characters of a document beyond which it's loaded without formatting. It's calculated by the size of the file, not counting the characters, so it could be a bit inaccurate. Formatting can be activated afterward.
- The possible hiding of the title and todo list is now restored when the app is run again.

#### Version 1.2.5

- The sorting functionality in the tables now sorts the numbers as such and not as letters, and the sorting stops before a possible formula in all the columns. If numeric and textual contents are both present, the numbers are shown first.
- Bugs fixing: the file search form was not working as expecting.

#### Version 1.2.4

- Added a list of all the available shortcuts that are not related to menu items (see The `Help` - `Shortcuts list` menu item).
- Now the width of the columns of the grid of the last four files is remembered by the app.

#### Version 1.2.3

- Added the button in the search form to find the previous recurrence in the document.
- Added the shortcut `Meta` + `Shift` + `G` to find the previous recurrence in the document in the search form.
- Improved the speed of the todo items compilation in the todo form.
- Bugs fixing: the use of the search function could crash the app.
- Bugs fixing: when the status of a todo item in the todo form is changed, now the possible red color is set properly.

#### Version 1.2.2

- Opening one the last four files used, now it's restored also the last selected cell.
- In presentation mode, the headings are now highlighted like the other paragraphs.
- Minor graphics improvements in presentation mode.
- Bugs fixing: in some circumstances, the content of the grid could not be saved.

#### Version 1.2.1

- Bugs fixing: the `Meta` + `V` shortcut in the grid didn't set it as modified.
- Bugs fixing: a value pasted with the `Meta` + `V` shortcut in the grid was not considered by a possible function.
- Bugs fixing: the shortcuts `Meta` + `C`, `Meta` + `X`, `Meta` + `V` and `Meta` + `A` in the fields of the search form didn't work as expected.

#### Version 1.2.0

- Added files search. See above the `Files management` title for information.
- Moving the mouse over the label of the file path and name, at the bottom left, now shows a hint with the complete content, even if in the label it's truncated due to its length.
- Minor graphic improvements.
- Bugs fixing: when the path of the file was very long, the date and time label on the right was overwritten.

#### Version 1.1.14

- In presentation mode, the cursor is now hidden, and the selected paragraph is shown at the middle of the screen.
- In light mode, the default font color of code, links, etc. is now more readable. 
- Bugs fixing: sometimes, the look of the document was not updated properly.
- Bugs fixing: on opening one of the last four files, the last position of the cursor was not always properly recovered.

#### Version 1.1.13

- The grid has now 10000 rows and 105 columns.
- The `Insert link to file` menu item now allows to select and insert more files, and to include them in the text within the proper Markdown markers.
- Added shortcut `Meta` + `Opt` + `.` to transform a list with dashes (`-`) into a numbered list.
- Added shortcut `Meta` + `Ctrl` + `.` to transform a numbered list into a with dashes (`-`).
- Bugs fixing: pressing `Return` in an empty item of a list delete this item only if it’s followed by an empty paragraph.
- Bugs fixing: on opening one of the last four files, the last position of the cursor was not always properly recovered.

#### Version 1.1.12

- Bugs fixing: it was not possible to format in bold or italics the selected text if starting at the beginning of a new paragraph.
- Bugs fixing: the possible Markdown markers within Markdown links was not properly shown.

#### Version 1.1.11

- Added new functions in the tables grid beyond `sum`: `max`, `min`, `avg`, `count`. Now they are shown in the color set by the user for the code.
- Added a sorting radio item in the `Words recurrence form`.
- Bugs fixing in the `Words recurrence form`.
- Bugs fixing in the tables grid.

#### Version 1.1.10

- Added the `Show words recurrence form` menu item and functionality (see above for information).
- In the grid, the `Return` key pressed in a cell at the left end of a table doesn’t select the following cell at its right but the first cell of the following row, in the second column.

#### Version 1.1.9

- Citations now have a lighter or darker background, while having the same font size.
- Bugs fixing: the formatting within the citations was not effective.
- Minor bugs fixing.

#### Version 1.1.8

- New shortcut in the grid `Meta` + `Opt` + `Shift` + `I` to add a new column in the current table.
- New shortcut in the grid `Meta` + `Opt` + `Shift` + `Backspace` to delete the current column in the current table.
- The shortcut in the grid `Meta` + `Arrow right` now moves to the last edited column of the current row.
- The shortcut in the grid `Meta` + `Arrow down` now moves to the last edited row of the current column, if the selection is not in the first column.
- Bugs fixing: `Backspace` erased the whole content of the current cell even when the editor was active.
- Minor graphic improvements.

#### Version 1.1.7

- The procedure of the menu item `Edit` - `Show duplicate words` now requires no confirmation.
- Minor bugs fixing and graphic improvements.

#### Version 1.1.6

- `Backspace` deletes the content of the selected cells after confirmation.
- The procedure to check possible repeated words in the current paragraph and the previous one is now much faster.
- Bugs fixing: the procedure to check possible repeated words in the current paragraph and the previous one didn’t work always as expected.

#### Version 1.1.5

- When a `.cvs` file is loaded, the grid is not shown automatically, but in the status bar at the bottom, after the name of the file, a sign of a diamond is shown to alert the user that there are some data inside it.
- Added the possibility to show the sum of a column of numbers in the grid.
- Bugs fixing: when the grid was modified in a new file without a name, it was possible to exit the app without being asked to save the file.

#### Version 1.1.4

- Bugs fixing: it was not possible to search for a table name in the grid.
- Bugs fixing: in some circumstances, the last line of the text was not properly updated.

#### Version 1.1.3

- Bugs fixing: when a new Markdown file was created, a grid file was created as well even if there were no data inside it.
