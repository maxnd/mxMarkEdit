<img align=“left” src=“https://github.com/maxnd/mxMarkEdit/blob/main/icon128.png”   width=“64”>

# mxMarkEdit

Version 1.4.8, published on July 8 2026.

Author and copyright: Massimo Nardello, Modena (Italy) 2024 - 2026.

🔶 mxMarkEdit is a local-first, native macOS Markdown editor designed for power users, writers, and academics. Deeply inspired by Emacs Org-mode, it seamlessly blends distraction-free text editing with inline visual formatting, a native bibliography manager compatible with Zotero, a integrated Excel-like data grids and comprehensive task management.

---

[Read this manual in PDF](https://github.com/maxnd/mxMarkEdit/raw/main/manuals/mxmarkedit-user-manual-en.pdf).

[Read the Italian translation of this manual in PDF](https://github.com/maxnd/mxMarkEdit/raw/main/manuals/mxmarkedit-user-manual-it.pdf).

The official [YouTube channel](https://www.youtube.com/@mxnrd) of the app contains some English and Italian videos generated with Google NotebookLM about the use of the app by university professors and students, and also by corporate managers and teams.

The PDF manual may be downloaded also within the app, with the `Help - Download manual` menu item.

For troubles in using the app, see below the *Troubleshooting* section.

See in the [wiki section](https://github.com/maxnd/mxMarkEdit/wiki) some nice presentations on why and how a manager, a university professor, a student and a corporate team might want to use mxMarkEdit.

😀 Do you use mxMarkEdit, and you feel that it has far too many shortcuts and functionalities that definitely make it not for humans? Download [this nice wallpaper](https://github.com/maxnd/mxMarkEdit/raw/main/wallpaper/wallpaper-mxmarkedit.png) to show everybody why!

---

mxMarkEdit is a free software for Mac for writing texts and todo items in [Markdown format](https://pandoc.org/MANUAL.html#pandocs-markdown) and easily exporting them to other formats with [Pandoc](https://pandoc.org), if it’s installed in the system in use. In each document, it’s available an Excel-like grid useful to manage various sets of data, and a file manager to search for a sentence in all the documents or spreadsheets contained in a folder. It’s also possible to use the app as a simple presentation manager. Finally, mxMarkEdit has a built-in bibliographic manager which may use the bibliography stored in the grid to compile the citations within a document in a way that resembles the one used by BibLaTex. Data from Zotero may be imported directly.

The software has been written in [Lazarus](https://www.lazarus-ide.org) with [Free Pascal](https://www.freepascal.org) and is released under the GPL version 3 license or following, available in www.gnu.org/licenses/gpl-3.0.html. The software runs in English or Italian.

mxMarkEdit *doesn’t use Electron* because of its performance and [security issues](https://medium.com/@flynn.kelseyelizabeth/dont-use-electron-until-you-ve-read-this-article-52d30401303d). The formatting of Markdown text is made by an original algorithm written in Free Pascal.

mxMarkEdit has been deeply influenced by [Org-mode](https://orgmode.org). Although it has far less functionalities and customizations than the latter, it’s a standard Mac app which benefits of Apple artificial intelligence and spell checking, and uses standard shortcuts.

### Privacy

mxMarkEdit does not collect any user data and does not access autonomously to his or her pictures, files or any other personal data. The data created with the app are stored only on the computer of the user and possibly on his or her cloud.


## Download and install

In the `app` directory of the source code there is an app compiled for Mac with the Silicon chip (M1 or following) ready to be copied in the `Application` directory and run. 

The app is *not* notarized by Apple anymore. To run it, see the [Apple instructions](https://support.apple.com/en-us/102445) (section “If you want to open an app that hasn’t been notarized or is from an unidentified developer”), or simply run in the terminal `xattr -r -d com.apple.quarantine /Applications/mxMarkEdit.app`.

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

The same presentation optimized with `Tools - Optimize presentation...` and shown in presentation mode looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot6.png)

The form that summarises the todo items typed in the document looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot3.png)

The form of the options of the app looks like this:

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/screenshot4.png)

Screenshots of the app used as meeting manager with fancy data

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/meeting_manager_1.png)

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/meeting_manager_2.png)

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/meeting_manager_3.png)

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/meeting_manager_4.png)

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/meeting_manager_5.png)

![](https://github.com/maxnd/mxMarkEdit/blob/main/screenshots/meeting_manager_6.png)

## Formatting

Some Markdown markers are hidden, as the text that follows them or is contained within them is properly formatted. This behavior may be changed in the options of the app. These markers are:

* headers (beginning with `#`, `##`, `###`, `####`, `#####` and `######`);
* italics (`*` or `_`), bold (`**` or `__`) and bold italics (`***` or `___`);
* code, shown in monospace font (`` ` ``);
* citations, which start with the character `>` followed by a space, that are shown with a lighter or darker background, according to the mode in use.

Other Markdown markers are not hidden, but are shown in a different color defined by the user:

* lines of code, shown in monospace font, among two markers `` ``` `` at the beginning of a paragraph;
* footnotes, like `[^1]` and `[^1]: Text of the footnote`;
* inline footnotes, like `^[Text of the footnote.]`;
* links to websites or files, like `[my web site](www.mywebsite.com)`;
* links to pictures, like `![my picture](img.jpg)`.

To show temporarily a picture in the text after its link or in a another window, see below for the proper shortcuts.

The markers cannot be escaped as far as the formatting of mxMarkEdit is concerned. Pandoc should instead manage them properly.

To have a Markdown document properly exported in other formats with Pandoc, the paragraphs must be separated by an empty line, except in the lists, numbered or not.

When a paragraph starts with a list heading (`*`, `+`, `-` or a number followed by a dot and a space), the `Return` key creates a new paragraph with the same heading. The numbered lists are properly renumbered if necessary.

To move a heading and all its relative content, included possible headings of lower level (that is, having more `#` at the beginning), in another position of the text, move the cursor within this heading without doing any selection and cut all in the clipboard with the shortcut `Meta` + `Shift` + `X`. The text in the clipboard can then be pasted elsewhere.

Links are properly recognized and formatted if the formatting is not disabled. Anyway, if they contain some Markdown markers, like `*` or `_`, these markers are hidden, although they are still present and the link works normally when clicked upon. To show the link with all its characters, even possible markers, format it as a line of code including it among two `` ` ``. Anyway, when the link is formatted as a proper Markdown link, like `[my web site](www.mywebsite.com)`, the possible Markdown markers are properly shown.

The formatting should work instantly, without delaying the typing, at least up to about 250,000 characters (test made in a Mac Pro with M1 chip and 8 GB of RAM). To work speedily with bigger documents, disable the formatting and, if necessary, also the titles and todo list (see below the `Edit` menu notes). In this way, even a text of few millions of characters may be modified without any delay in typing.

By default, the app deactivate text formatting for documents bigger than 250,000 characters. This value can be modified in the options of the app (see below) to fit the performance of one’s own Mac. The size of the document is calculated by the size of the file, not counting the characters, so it could be a bit inaccurate.

The inline footnotes should be preferred over the traditional ones, because deleting a reference of a footnote and not the corresponding text or vice versa could compromise the match between other footnotes and their references.

## Titles and todo list

At the left of the text, there’s a list of the titles (starting with `#`) and todo items (starting with `– [ ]`  or `– [X]`). By clicking on an item in this list, the cursor moves to the corresponding title or todo item. While moving the cursor in the text, the corresponding title or todo item is highlighted.

Above this list there is combo box in which it’s possible to select the level of the headings to be shown. The todo items and the heading under which the cursor is located are always shown.

In the todo items, it’s possible to add a deadline. The format of the date should be `year-month-day`, where month and day are always of two digits. The date must be followed by 3 separator characters before the title of the todo item. For instance: `- [X] 2024-12-05 • Buy some food`. The shortcut `Meta + T` mentioned also below insert automatically the todo marker and the deadline in the proper format with a delay in days that is specified by the user in the options of the app (the default is 7 days).

To assign a todo item to one or more resources (persons), insert their names everywhere in the title of the item preceded by `@`; each resource name must end with a space, a period, a comma, a colon or a semicolon. For instance: `- [X] 2024-12-05 • @Mark, @John - Buy some food`, or `- [X] 2024-12-05 • @Mark; @John. - Buy some food`. It will be possible to filter the todo items by resource in the todo form.

## Tables

At the bottom of the text there’s an Excel-like grid of 105 columns and 10,000 rows which may contain simple tables, which are horizontal portions of this grid. This is hidden by default, and can be shown dragging up with the mouse the splitter at the bottom of the text, or with the shortcut `Meta` + `Shift` + `T`. In the first column on the left, named `Tables names`, the user must type the name of a table (e.g. `Books`), and in the columns at its right (`A1`, `B1`, etc.) all the necessary fields (e.g. `Author`, `Title`, `Year`, etc.). The name in the first column is shown with the color of the `headings 2`, while the fields have the color of the `headings 3`. Below the fields, it’s possible to insert any kind of data. It’s possible to add more tables in the grid, adding other names under the `Tables names` column followed by some fields, like in the second screenshot above. To mark the bottom row of a table not followed by another table, type something in the `Tables names` column just after it.

If it’s necessary to insert dates in a field, use the format `year-month-day`, where month and day are always of two digits (e.g. `2025-01-05`), so that the sorting on this field will produce a correct result. See below for some shortcuts useful to insert and change the dates.

The content of the grid is saved in a separate file with the same name of the one in use, but with the `.csv` extension. In this file the items are separated by tabs, and it may be easily imported in a spreadsheet. When the current document is exported to Pandoc, if there are some data in the grid, the software creates a new file with the extension `.export` which contains both the document and the tables properly formatted in Markdown format. In the converted document in Word or Writer format, the tables are located at the end of the text.

When a `.cvs` file is loaded, the grid is not automatically shown, but in the status bar at the bottom, after the name of the `.md` file, it’s shown the label `& .csv`, to alert the user that there are some data inside the grid.

In the grid, it’s possible to select more cells dragging the mouse or holding the `Shift` key and pressing the arrow keys. The selected text can be copied in the clipboard and pasted in another position of the grid or in a document, or deleted. It’s possible also to paste some data copied from a spreadsheet like Excel or Numbers. See below for the shortcuts useful to do that. It’s also possible to auto resize the width of the columns with a double click at the right of their headings.

To edit easily a cell in the grid, it’s available an extended editor activated with the shortcut `Ctrl + Space`. In this editor no formatting is available, but the links are properly recognized. At the left bottom there are 4 buttons to show the cell at the left, above, at the right and below the current one, mapped to the shortcuts `Meta + Opt + Left / Up / Right / Down arrow`. To quit the editor, use the `Close` button or press `ESC`.

At the bottom, there’s the `Find` field useful to search for data contained in the grid. Type within it the text to be found and press `Return` or the shortcut `Meta + G` to select the following cell after the current one in just the current field (column) of the current table that contains that text, or `Shift + Return` or the shortcut `Meta + Shift G` to select the previous cell before the current one in just the current field (column) of the current table that contains that text. Anyway, if the selected column is the first, containing the table names, the search will look for the following or previous table name. The search is case insensitive. To continue the search, select again the field and press `Return` or `Shift + Return`, or use the shortcut `Meta + G` or `Meta + Shift + G` when the grid is focused.

At the right of the `Find` field there is the `Filter` field useful to filter the rows of the current table. To activate the filter, write something in this field and press `Return` to filter the rows of the current table which in the current field contain the typed text. Other filters will be added to the existing one(s), to emulate the `and` condition. To clear the filters in the current table and calculate the formulas, clear the content of the `Filter` field and press `Return` or use the shortcut `Ctrl + Shift + F`.

In the `Find` and `Filter` fields, numbers must be typed with possible thousand separators.

If a cell contains a formula, in the cell below it will be reported the result of the calculation of all the numbers contained in the previous cells of the same field (column) and of the same table. The result will be updated when a value in those cells will be changed. Textual contents and improperly formatted numbers will not be considered. The name of the table and of the field (column) must be specified, otherwise no result will be shown.

The formulas are the followings:

- `------` or `---sum`: reports the sum of the numbers;
- `---max`: reports the bigger number;
- `---min`: reports the smaller number;
- `---avg`: reports the average of the numbers;
- `---count`: reports the total number of the rows, also nonnumeric;

The formulas are shown in the color set by the user for the code (see below the option of the app for more information). The possible filter doesn’t hide the formulas and the following result fields, and the rows not filtered are not included in the computations. Deleting a row activate the computation of the formulas, but not move a row in another table. Before closing the document, remove the filters from each table, so that the result fields are saved with the values relative to all the data of the column.

It’s possible to sort the content of the current column of the current table with the shortcut `Meta` + `Ctrl` + `S` (ascending) or `Meta` + `Ctrl` + `Shift` + `S` (descending). Numeric content are put before textual ones, and the possible empty lines before numbers. If a formula is present in any column, the sorting is limited to the rows before it. If no formula is present and the table is not followed by another table, with a name in the table names column, it’s necessary to mark the bottom row of the current table typing something in the `Tables names` column just after it.

To replace the content of the grid with the tables contained in a `.csv` file created with mxMarkEdit, use the menu item `File - Import tables...`. To delete the content of the grid so that it’s not loaded any more, delete the related file with the `.csv` extension when it’s not loaded.

See below for some useful shortcuts available in this grid.

## Bibliographic management

mxMarkEdit may be used to manage a bibliography and to insert citations of the books and articles into a document appropriately formatted, in a way similar to BibLaTex. To use this feature of the app, proceed as follows.

### Creating one or more bibliographic tables

In mxMarkEdit, the tables containing a bibliography must be structured as follows.

The first column on the left must contain the name of the table, as usual. Several bibliographic tables may be inserted in sequence with different names to structure the bibliographic material in various areas.

The second column (A1) must contain the key of the work, that is, some words or numbers that identify it uniquely within all the tables contained in the grid. It is suggested to use the surname of the first author followed by a space and the year of publication. If there are multiple works by the same author published in the same year, a progressive lowercase letter may be added after the year. For example, to report the bibliographic data of a book written by R. Taylor in 2024, the key could be `Taylor 2024`. If there were multiple works published by the same author in 2024, the subsequent works would have the key `Taylor 2024a`, `Taylor 2024b`, etc. It is the user’s responsibility to verify that the key assigned to a work is unique, that is, it has not already been assigned to another work. To do this, use the shortcut `Meta` + `Ctrl` + `K`, active just in this column, to make it unique. If the key is composed as `surname + year`, sorting the works by key will sort them by author and then by year.

The third column (B1) must contain the author or authors of the work with the formatting required for the bibliography that will be inserted at the end of the document. In order for the authors to be ordered alphabetically, their surname must be inserted before their name.

The fourth column (C1) must contain the author or authors of the work with the formatting required for footnotes. Normally, the name is bulleted and precedes the surname.

The fifth column (D1) must contain the full title of the work; it’s necessary to insert the italics markers (asterisks) if needed.

The sixth column (E1) must contain the abbreviated title of the work, which will be used for citations of a work after the first; it’s necessary to insert the italics markers (asterisks) if needed.

The seventh column (F1) must contain the details of the citation for the bibliography, such as the publisher, place of publication, year and total pages (not relating to the single citation) in the case of articles or miscellanies.

The eighth column (G1) must contain the details of the citation for the footnotes, such as the publisher, place of publication and year.

In the following columns can be freely insert the data considered useful, also using the extended editor that is activated with `Ctrl + Space`. For example, it may be needed a field in which to insert comments on the work, even of a certain length, or the name of the relative PDF file, a link to the publisher’s website, and so on. The columns after F1 will be ignored by the app for the purposes of compiling citations.

It’s possible to import one or more citations from Zotero in the form `Zotero`, shown by the functionality `Tools` – `Zotero...`. Even if the access to Zotero database is read only, so there is no possibility to change it, it’s better to work on a copy. In any case, check that the path and the file name at the left bottom of the form corresponds to the original Zotero database or to a copy.

In one or both fields `Title contains` and `Author contains`, type some text to be searched for in the Zotero database and press `Return` or click on the button `Find`. The results, that is, the author names and the titles, are shown in the top left grid. Select an item in this grid to have the full citation compiled in the citation field below the grid for the columns A1 – E1, according to the mentioned pattern: the key, the authors and the titles. Check the `Author initial name` checkbox to have only the author initial names and `Titles in italics` to have the titles in italics. Then compile manually the details of the citation for the column F1: double click or press `Return` on the various useful items in the top right grid to have them added in the citation field separated by `|` among two spaces. The field `pages` will be preceded by `#`, which will be automatically removed when inserting the quote in the grid. The text of the quote may be changed directly if necessary. To insert the content of this field in a new row of the grid, click on the button `Insert in grid` or use the shortcut `Meta` + `I`. The key will be made unique automatically. The G1 column will be compiled as the F1, except the pages of articles or sections of books.

Finally, it’s possible to keep and update all the bibliography in just one `.cvs` file to avoid scattering it in many files. Then it’s easy to update the bibliographic tables of each other file in use with the latest version of data importing that main bibliographic file with the menu item `File - Import tables...`.

### Inserting citations in the text

To insert a citation of a work in the document, normally in the footnotes, just insert its identification key between two curly brackets, followed by all the detailed elements necessary for the single citation. For example, `Cf. {Taylor 2024}, pp. 23-46.`. It’s necessary to be careful in typing correctly the key of the work to be quoted, respecting uppercase and lowercase. To do this, just display the bibliographic table, identify the key to be reported, copy it in clipboard with the shortcut `Meta + Opt + C` and paste it into the text.

To have visual feedback of the citation related to a key without opening the grid, use the shortcut `Meta + Shift + K` when the cursor is inside the key, that is among the two curly brackets that surround it. Press `Esc` or click outside the preview to close it.

If it’s necessary to quote an ancient text contained in a modern edition and to mention a number or chapter of its passage after the original title, use the key just for the modern work, and quote manually the first. For instance: `S. Augustine, *De Civitate Dei* I, 1-36, in {Augustine 2020}, 50.`. Of course, only the modern edition will appear in the bibliography.

### Producing a document with bibliography

Once the document has been completed and the keys of the various works have been inserted, the menu item `Tools - Compile bibliography` can be used. In this way, mxMarkEdit creates another file in the same folder as the one in use, with the same name and the extension ` - with bibliography`, convert it with Pandoc and open it with the word processor without adding the possible content of the table at the end of the text. In this file, the various keys are replaced by the actual citations of the works, in the following way.

- The first citation of a work is composed by associating the contents of columns C1, D1 and G1.
- The second citation of a work is composed by associating the contents of columns C1, E1 (abbreviated title) and G1.
- If a citation is about the same work as the immediately preceding citation, the word `Ibidem` is inserted in italics in place of its bibliographic data.
- The bibliography is inserted at the end of the text. It contains all and only the works cited in the document, and is created associating the content of columns B1, D1 and F1. The bibliography is ordered alphabetically.
- The contents of the B1 and C1 columns are followed by the `Author separator`, specified in the options.
- The contents of the D1 and E1 columns are followed by the `Title separator`, specified in the options.
- To have the authors formatted in small caps in the text exported with Pandoc, select `Authors in small caps` in the options. To have the same formatting for some text in other fields, use this format: `[M. Regan]{.smallcaps}`.

In citations, if an author is the same as the previous one, it is replaced by `Idem`. It is the user's responsibility to verify that the two authors are not homonyms but the same person.

If a `Ibidem` is followed by the same page numbers of the previous citations, they may be removed manually if this is required by the editorial rules.

It’s not possible to automatically remove the author’s name alone from citations following the first, leaving only the author’s last name. As a workaround, insert a character not used elsewhere in the document, such as `%` or `$`, into column E1 at the beginning of the abbreviated title and after any possible asterisk. Then, once the document is exported to Word or Writer, search for every single occurrence of that character, remove it, and manually correct the author's name immediately preceding it.

In case the citations need to be formatted differently to correspond to other methodological requests, correct only the content of the columns and then to regenerate the document with bibliography. So, there’s no need to correct all the individual citations contained in the document.

### mxMarkEdit and other reference managers

As is apparent from these notes, the bibliographic manager of mxMarkEdit, while not having the functionality of more specialized software such as Zotero and Mendeley, is a much simpler and more usable tool, both for those who plan to dedicate themselves to research only temporarily, to simply write a dissertation, and for those who find the system of composing citations based on style sheets too complex and dangerous.

This approach allows to reformat all the citations of a document simply by changing the style sheet in use, but it becomes tricky when there’s no style that perfectly matches the methodological needs of one’s institution or publisher. Furthermore, there is always the risk that in particular cases the style sheet does not give the desired results. The approach used by mxMarkEdit is much more solid, as the citation, even the part that follows the title, is composed by the user in its final form, and therefore is certainly correct. Even if this involves the need to rewrite it if it’s necessary to adapt to other methodological requests, the user never runs into dangerous malfunctions.

So, it's possible to use Zotero as main bibliographic manager and import from its data all the items that are relevant for a specific article or book inserting them in a bibliographic table with the procedures shown above. In this way, one sticks with a solid and fully featured bibliographic manager but may use at the same time the simpler and secure bibliographic system of mxMarkEdit to compile the citations.

Finally, the mxMarkEdit bibliographic database, being contained in a `.csv` file, can always be read and modified, maybe by writing some code, so that it can be imported into other software.

## Presentations

it’s possible to use the app for presentations:

1. load a document, possibly with some tables, containing the sequential text of the slides in Markdown format, with many paragraphs under titles and subtitles, like a normal text;
2. make mxMarkEdit window full screen and enlarge possibly the font size of the document so that it can be seen easily by the participants;
3. share the interface of mxMarkEdit with a video projector or on a video call with apps like Webex, FaceTime or Zoom;
4. use the shortcut `Meta` + `E` to begin the presentation showing the paragraph following the current one, or `Meta` + `Shift` + `E` to do that showing the paragraph previous the current one, both in black or white, according to the theme in use, skipping the empty lines, and fading the other text; when the presentation mode is enabled with one of these two shortcuts, the following or previous paragraphs may be selected also with the shortcuts `Meta` + `Arrow down` and `Meta` + `Arrow up`;
5. comment the content of the paragraph in black or white; the participants see the text that is currently focused, and its position in the structure of the document in the left bar, which anyway can be hidden;
6. if the reference to some tables is needed, show the grid, locate the table and comment its data with the participants using possibly the zoom functionalities of Mac to make visible the small numbers.

In presentation mode, the status bar is hidden, and the spell checking is automatically disabled. To quit the presentation mode, press `Esc` or click anywhere in the text. Possibly, reenable the spell checking.

If in the document there is a link to a picture at the beginning of a paragraph and properly formatted (e.g. `![Data on sales](file:///Users/massimo/Downloads/sales-data.jpg`), that picture will be shown during the presentation in a wider window. To close the picture, click on it or press `ESC`.

Keep the presentation in standard Markdown format, to be able to export it with Pandoc without any trouble, and then create a copy of it optimized for mxMarkEdit with the menu item `Tools` - `Optimize presentation...` if it has to be presented with mxMarkEdit.

To export an mxMarkEdit presentation in PDF so that it can be shown on any computer, save all the screenshots from the current position to the end of the document with `Meta + Ctrl + E`; the text will be scrolled automatically, and it will take 2 seconds to get each screenshot; press `ESC` to interrupt the process. Then select all the screenshots in the `Download` directory, `Ctrl + click` on them and choose the `Quick Options - Create PDF`. The screenshots are numbered progressively. To restart the number of the ones to be created, restart the app. The pictures will not be shown nor exported.

## Files management

The `File - Search in files...` menu item open a form to search a word or a sentence within all the `.md` (document) and `.csv` (tables) files contained in a folder specified by the user, including its possible subfolders. Specify the folder to search in with the button `Set folder`, whose value will be remembered by the app, and then type the text to search for in the field `Find`. Then press `Return` or click on the button `Find`.

The `\n` code finds the `Line feed`, which in macOS or Linux are the paragraph breaks, the `\r` code finds the `Enter` while the `\t` code finds the tabs. In document written on Windows systems, the paragraph breaks are made by `Line feed` + `Enter`.

In the grid above the buttons it will be shown a row for each occurrence of the text to be found both in the documents and in the tables files, along with the context in which it appears (some words before and after it). The columns of the grid – path, file name and context – can be sorted with a click on their headers. Press `Return` on a row of the grid, or double click on it to open the corresponding file and to move the cursor to the position of the text to be found, or, if the file is a `.csv` one, to the cell that contains it.

The search in files returns maximum 100 recurrences for each document or spreadsheet analysed, and stops at 5,000 global recurrences, asking the user to possibly specify more words to find. Emojis too can be searched for. Press `Esc` to stop the search.

Moving the mouse over the label of the file path and name, at the bottom left of the main interface of the app, a hint appears with the complete content, even if in the label it’s truncated due to its length.

## Using the app in a team

The app can be used in a team, giving team members access to the various files. To keep track of who made which changes, use management software like GitHub, manually downloading the files to one's Mac, editing them, and then uploading them again. The app doesn't have any features to simplify this process.

More simply, share the various files on a cloud or a shared folder on a server on a local network, ensuring everyone can access them and only some can edit them. Read-only files should be opened using the `File – Open as read-only...` menu item. If they were opened normally, the app, unable to save them to their original location, might not quit and prompt the user to save them to another location. This option is intended for personal use of the app, to avoid the risk of losing data if it's not possible to save them to their original location.

## Menu items

The last eight opened files are available in the `File` menu. When they are opened, the cursor move to the last position it had during the last editing, and the width of the columns of the grid is restored.

The `File – Open as read-only...` menu item opens a file without giving the user the possibility to edit it. The file name is not added to the list of recently opened files.

The `Edit - Find` menu item opens the standard Apple panel to find and replace text. See below for some useful shortcuts to use these functionalities. 

The `Edit - Insert link to file` menu item allows to select one or more files and to insert their paths and names in the text within the Markdown markers and preceded by the `file://` heading. All the possible spaces are converted with `%20`, so that the file name may be properly recognised as a link. It’s not possible to insert relative links due to macOS restrictions.

The `Edit - Show current title or todo` menu item shows at the top of the left list, after five other items if existing, the title or todo item the cursor is under to.

The `Edit - Show todo form` menu item opens a form with the list of all the todo items. Pressing `Return` or double clicking on one of them selects the corresponding item in the document. Click on the headers of the columns of the form sorts the data, initially sorted by deadline. The date of the expired items is shown in red. The combo list at the bottom allows to show only the todo items assigned to a resource. The check box at the bottom allows to hide the todo items already done. The button `Toggle state` changes the state of the selected todo item from to be done to done, and vice versa. The button `Copy` copies into the clipboard the visible todo items; the values are separated by tab, so that they may be pasted in the grid or in a spreadsheet. See below for the shortcuts active on this form.

The `Edit - Show duplicate words` menu item shows in red all the words of each sentence that are present twice in the same sentence or in the previous one, to alert the user to use possibly a synonym. Moving the cursor or clicking somewhere in the text remove the red color.

The `Show words recurrence form` menu item opens a form with the list of all the words used in the current document, excluded the possible YAML headings (`title:`, `author:` etc.), and their recurrence, that is how many times they appear in the document. By default, the list is sorted by recurrence, but it’s possible to click on the radio item `Sort by words`, at the right bottom, to sort the list by name. Below the grid there is a field in which it’s possible to type the words that should not be considered, like articles, separated by commas. These words are automatically sorted. Below this field, there are some buttons to include the word selected in the grid within the words to skip, to update the results after having changed the words to skip, to copy in the clipboard the content of the grid and to close the form.

The `Edit - Disable formatting` menu item prevents the formatting of the text, except the normal font name, its color, size and line spacing, greatly improving the performance of the app with very large documents. In the options, it’s possible to set the number of characters of a document beyond which it’s loaded without formatting, that is just activating automatically this functionality.

The `Tools - Optimize presentation...` menu item creates a copy of the current presentation optimized to be shown with mxMarkEdit, and saves it with the name of the file in use plus ` - mxMarkEdit` addition. Basically, the various items of first level are separated by an empty paragraph, and the repeated titles of the slides are removed.

The `Tools - Convert with Pandoc` menu item converts the current document and its possible tables into another format specified in the options of the software. The converted file is created in the same folder of the current document.

The `Tools - Convert with Pandoc without table` menu item converts the current document without its possible tables into another format specified in the options of the software. The converted file is created in the same folder of the current document.

The `Tools - Open another window` menu item opens another instance of the software if the app is in the `Applications` directory.

The `Tools - Transparency` menu item activate or deactivate two levels of transparency of the interface of the software, giving the impression to watch or type on a dark or clear glass, according to the mode in use. This functionality is just for aesthetic reasons.

The `Tools - Options` menu item opens the options of the app, where are specified 

- the name and the color of the font of the text;
- the name and the color of the font for code;
- the color of the links;
- the color of the footnotes;
- the color of the todo items;
- the color of the first level titles; 
- the color of the second level titles;
- the color of the other levels titles;
- the size of the normal font;
- the size of the code font;
- the possibility to show the Markdown markers while leaving active the formatting;
- the line spacing value (1.0 is the default);
- the delay in days for the new deadlines (7 is the default);
- the number of characters of a document beyond which it’s loaded without formatting; it’s calculated by the size of the file, not counting the characters, so it could be a bit inaccurate;
- the separators added in the citations between the author(s) and the title;
- the separators added in the citations between the title and the details;
- the option to make the author(s) in small caps in the Pandoc output file;
- the Pandoc options;
- the path of the Pandoc executable;
- the possible template (a file of Word, PowerPoint, Writer, etc.) to use as a reference;
- the format of the file the note should be converted to (`.docx`, `.odt`, etc.);
- two buttons to reset the colors of the text in black and white or in various colors; these colors are different for dark and light mode.

A green or red circle is shown at the right of the fields of the path of the Pandoc executable and of the template to inform that the files are present or not. See the Pandoc manual for more information.

The `Help` - `Shortcuts list` menu item shows a list of all the shortcuts available that are not related to menu items.

The other menu items are self-explaining.

## Shortcuts

There are also some shortcuts beyond the ones related to the menu items.

### In the main form

* `Meta` + `Z`: undo the last action in the document (not in the grid).
* `Meta` + `Shift` + `Y`: redo the last action in the document (not in the grid).
* `Meta` + `Opt` + `1-6`: select one of the six filter options above the title and todo list.
* `Meta` + `Ctrl` + `1-6`: set the current paragraph as 1-6 header.
* `Meta` + `Ctrl` + `0`: remove the header marker from the current paragraph.
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
* `Meta` + `Ctrl` + `.`: transform a numbered list into a list with dashes (`-`).
* `Meta` + `Shift` + `J`: set the current position of the cursor in the bookmark.
* `Meta` + `J`: move the cursor to the position already set in the bookmark.
* `Meta` + `Shift` + `P`: show in a form the picture contained in a Markdown image link at the beginning of the current paragraph (Esc to close).
* `Meta` + `Shift` + `Opt` + `P`: show in the text the picture contained in a Markdown image link at the beginning of the current paragraph.
* `Meta` + `E`: show the following paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Shift` + `E`: show the previous paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Arrow down`: if presentation mode has been enabled with `Meta` + `E` or `Meta` + `Shift` + `E`, show the following paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Arrow up`: if presentation mode has been enabled with `Meta` + `E` or `Meta` + `Shift` + `E`, show the previous paragraph in black or white, according to the theme in use, skipping the empty lines, and fades the other text; this is useful to use mxMarkEdit for presentations. See above for details.
* `Meta` + `Ctrl` + `E`: save all the presentation screenshots from the current position to the end of the document in the `Downloads` directory.
* `Meta` + `U`: make uppercase the current word.
* `Meta` + `Opt` + `Shift` + `U`: make lowercase the current word.
* `Meta` + `Opt` + `U`: capitalize the current word.
* `Meta` + `Shift` + `T`: show the tables grid.
* `Meta + Opt + C`: copy the selected citations so that they can be properly pasted in the bibliographic table.
* `Meta + Shift + K`: show the citation taken from the bibliographic table that matches the current key.
* `Meta` + `Shift` + `X`: if the cursor is within a heading, cut in the clipboard the same heading and all the text that is under it, included possible headings of lower levels; this text may be pasted elsewhere;
* `Meta` + `Shift` + `F`:
  * within a footnote reference in the document (e.g. `[^1]`), move the cursor to the corresponding footnote;
  * within a footnote (e.g. `[^1]: This is the text of the footnote.`), move the cursor to the corresponding footnote reference in the document; 
  * in other positions, create a new footnote reference and a new footnote, both properly automatically numbered.

### In the tables grid

* `Ctrl` + `Space`: open the extended editor.
* `Meta` + `Opt` + `Arrow left`: in the extended editor, show the cell at the left of the current one.
* `Meta` + `Opt` + `Arrow top`: in the extended editor, show the cell over the current one.
* `Meta` + `Opt` + `Arrow right`: in the extended editor, show the cell at the right of the current one.
* `Meta` + `Opt` + `Arrow down`: in the extended editor, show the cell below the current one.
* `Meta` + `Opt` + `C`: copy the bibliographic key contained in the second column among curly brackets.
* `Meta` + `Ctrl` + `K`: make unique the current key contained in the second column.
* `Meta` + `Shift` + `I`: insert a new row.
* `Backspace`: delete the content of the selected cell or cells, after confirmation.
* `Meta` + `Shift` + `Backspace`: not on a table name, delete the current row, after confirmation if it’s not empty.
* `Meta` + `Shift` + `Backspace`: on a table name, delete it after confirmation.
* `Meta` + `A`: on a table name, select it.
* `Meta` + `G`: search downward the text typed in the `Find` field starting from the current position and just in the current column (field) of the current table.
* `Meta` + `Shift` + `G`: search upward the text typed in the `Find` field starting from the current position and just in the current column (field) of the current table.
* `Meta` + `Opt` + `Arrow up`: move up the current row.
* `Meta` + `Opt` + `Arrow down`: move down the current row.
* `Meta` + `Opt` + `Shift` + `I`: add a new column in the current table, after confirmation.
* `Meta` + `Opt` + `Shift` + `Backspace`: delete the current column in the current table, after confirmation, if it has a title and if it’s not the first one, containing the tables names.
* `Meta` + `Opt` + `Arrow left`: move left the current column (field) just of the current table (not the entire column of the grid) with its content.
* `Meta` + `Opt` + `Arrow right`: move right the current column (field) just of the current table (not the entire column of the grid) with its content.
* `Meta` + `Ctrl` + `Arrow up`: move the current table, with all its field, before the previous one.
* `Meta` + `Ctrl` + `Arrow down`: move the current table, with all its field, after the following one.
* `Meta` + `Arrow up`: in the tables names column, select the previous table title, while in the other columns move the cursor to the top of the grid.
* `Meta` + `Arrow down`: in the tables names column, select the following table title, while in the other columns move the cursor to the last edited row of the current column.
* `Meta` + `Arrow left`: move the cursor to the first column of the grid.
* `Meta` + `Arrow right`: move the cursor to the last right edited column of the current row.
* `Ctrl` + `D`: insert the current date except in the tables names column and in the headings.
* `Ctrl` + `Arrow right`: if the current cell contains a date, increase it of one day.
* `Ctrl` + `Arrow left`: if the current cell contains a date, decrease it of one day.
* `Meta` + `Ctrl` + `S`: sort ascending the content of the current column (field) in the current table, after confirmation; see `Tables` title above for more information.
* `Meta` + `Ctrl` + `Shift` + `S`: sort descending the content of the current column (field) in the current table, after confirmation; see `Tables` title above for more information.
* `Meta` + `Ctrl` + `F`: move the cursor in the search field, if it’s visible.
* `Meta` + `Ctrl` + `Shift` + `F`: move the cursor in the filter field, if it’s visible.
* `Ctrl` + `Shift` + `F`: remove the filters in the current table and calculate the formulas.
* `Meta` + `C`: copy the content of the selected cells in the clipboard.
* `Meta` + `V`: paste the content of the clipboard in the current (and possibly following) cells.
* `Esc`: undo the last changes while the editor of a cell in the grid is still active.

### In the Zotero form

* `Return`: on the detail grid, add the selected item.
* `Meta` + `I`: insert the citation in the grid.
* `Esc`: close the form.

### In the todo form

* `Meta` + `Opt` + `H`: hide the todo items already done.
* `Meta` + `T`: toggle the state of the selected todo item from to be done to done, and vice versa.
* `Meta` + `C`: copy in the clipboard the todo items.
* `Esc`: close the form.

### In the search files form

* `Meta` + `F`: move the cursor in the `Find` field.
* `Esc`: close the form.

### In the words recurrence form

* `Esc`: close the form.

## Backup

When a `.md` file that has been modified is closed, a backup copy is created with the `.bak` extension. The same happens for the `.cvs` file, with the `.cvs.bak` extension.

## Configuration files

The software creates these two configuration files that can be deleted to reset the configuration of the app, included the user defined colors:

- `/Users/[username]/Library/Preferences/mxmarkedit`
- `/Users/[username]/Library/Preferences/mxmarkedit.plist`

# Troubleshooting

- If the app cannot open the Zotero library in the `Documents` or `Downloads` directory, probably it's a matter of file permission. Open a `.md` file in this folder, give the app the permission to access the folder, quit and run again the app.
- If the title of a document contains a colon, Pandoc cannot translate it in Word, but simply nothing happens.

# Mentions and reviews

- [App Catalog](https://appcatalog.cloud/apps/mxmarkedit).
- [Free Download Manager](https://en.freedownloadmanager.org/Mac-OS/mxMarkEdit-FREE.html).
- [Indie Apps Catalog](https://indieappcatalog.com/app/991483088552/mxmarkedit).
- [LibHunt](https://www.libhunt.com/r/mxMarkEdit).
- [Mac Informer](https://macdownload.informer.com/mxmarkedit/).
- [MacUpdate](https://www.macupdate.com/app/mac/64986/mxmarkedit).


# Revision history

**Version 1.4.8**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.2.
- In citations, if an author is the same as the previous one, it is replaced by `Idem`.
- Added the new menu item `Tools - Convert with Pandoc without table`.
- Improvements of the documentation.

**Version 1.4.7**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.2.
- In the Zotero panel, the pages of items are now reported in the citation field preceded by `#`, which is removed when the citation is imported in the grid; in this way, the pages are identified and not inserted in the field of the details of the citations for the footnotes.
- Reduced the responsiveness of the grid scrolling.
- Improved Zotero panel look.

**Version 1.4.6**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.2.
- New shortcut `Meta` + `Shift` + `Backspace`: on a table name, delete it after confirmation.
- New shortcut `Meta` + `A`: on a table name, select it.
- Added titles in italics checkbox in the Zotero panel.
- Bugs fixing: when the authors are exported with Pandoc in small caps, also the possible “edited by“ sentence was in small caps.
- Improvements of the documentation.


**Version 1.4.5**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Improved error messages on Zotero connection.
- Added number of items in Zotero connection.
- Added use of tab to navigate among the Zotero grids and `Return` on details grid to add the selected one.

**Version 1.4.4**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Added Zotero integration.

**Version 1.4.3**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Numbers in data grid have thousand separator by default.
- Bugs fixing: in the data grid, number sorting didn't work with thousand separator.

**Version 1.4.2**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Added the thousand separator in the numbers within the grid.
- Reduced the responsiveness of the horizontal scrolling in the data grid.
- Corrections in the documentation.

**Version 1.4.1**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- New shortcut: `Meta` + `Ctrl` + `1-6`: set the current paragraph as 1-6 header.
- New shortcut: `Meta` + `Ctrl` + `0`: remove the header marker from the current paragraph.
- In the code, the unit files have been renamed properly.

**Version 1.4.0**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Added the possibility to open a file in read-only mode (see menu `File`).
- Bugs fixing: the shortcut for bold and italics didn't remove the existing formatting.
- Bugs fixing: when the document is empty and the grid has no data, the app doesn't ask to save data any more.

**Version 1.3.30**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Bugs fixing: open a `.md` file with a double click in Finder while the app is already running may retain old table data of another `.md` file.

**Version 1.3.29**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Added the inline pictures: in a picture link at the beginning of a paragraph, press `Meta` + `Shift` + `Opt` + `P`.
- Bugs fixing: the link to a `.png` picture was not created properly.
- Bugs fixing: in presentation mode, the links were not shaded.
- Bugs fixing: in presentation mode, the picture was not resized properly.

**Version 1.3.28**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Bugs fixing: opening a `.md` file in the app with a click on a link to that file didn't save the possibile changes to the current document.

**Version 1.3.27**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- In the files management, now `Esc` stops the search if active.
- The links to websites and files have now the same color of the link text.
- Corrections in the documentation.

**Version 1.3.26**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Small bugs in the titles list fixed.

**Version 1.3.25**

- Compiled with Lazarus 4.6 and FPC 3.2.2, tested on macOS Tahoe 26.5.1.
- Small bugs fixed.

**Version 1.3.24**

- Improved file loading speed.

**Version 1.3.23**

- Text search has been replaced with the standard Apple search panel.
- Small bugs fixed.

**Version 1.3.22**

- Small bugs in search have been fixed.

**Version 1.3.21**

- In the extended editor, there are 4 buttons to show the cell at the left, above, at the right and below the current one, mapped to the shortcuts `Meta + Opt + Left / Up / Right / Down arrow`.
- Bugs fixing: the composition of some citations were note correct.

**Version 1.3.20**

- In the composition of the citations, the details of the items for the footnotes are taken from the G1 column, while those for the bibliography are still taken from the F1 column.
- Bugs fixing: the composition of some citations were note correct.
- The documentation has been corrected and improved.

**Version 1.3.19**

- Added the `Author separator`, `Title separator` and `Authors in small caps` in the options of the app, to have a better management of the citations.

**Version 1.3.18**

- When `Compile bibliography` functionality is called, the output document is converted automatically with Pandoc in the defined format.
- In the composition of the citations, no comma is added by the app among the various parts of the citations. Commas or any other separator must be inserted by the user in the different columns; see above for details. This change has been done to make the citation system able to match the various requirements of the editors.
- Bugs fixing: the shortcuts `Mets + C`, `Meta + V` and `Meta + X` were not active during the editing of a cell in the grid.

**Version 1.3.17**

- Bugs fixing: the combo box above the title list didn’t change the selection of the titles immediately.
- Small improvements in the documentation.

**Version 1.3.16**

- At the bottom of the todo form, when the completed items are visible, now it’s shown also their number and their percentage with respect to the total number of items.
- Bugs fixing: pressing `Return` at the end of a todo item with a deadline but without any description didn’t delete it.
- Bugs fixing: toggling the state of a todo item in the todo form didn’t always work as expected.

**Version 1.3.15**

- The filters in the grid now are added to the possible existing ones, emulating the `and` condition.
- Added the shortcut `Ctrl + Shift + F` to remove the filters in the current table and calculate the formulas.
- Added some space at the beginning and at the end of the text to create some distance from the top and bottom borders of the text field.

**Version 1.3.14**

- Added in the options of the app the possibility to show the Markdown markers while leaving active the formatting.
- In the table, the rows with the tables and field names are now slightly highlighted.

**Version 1.3.13**

- Bugs fixing: when a cell in the grid was edited, the text was initially selected and the left and right keys where not able to move the cursor to the beginning or end of the text.

**Version 1.3.12**

- In the grid, now the filter doesn’t hide the formulas and the result fields, and the rows not filtered are not included in the computations.
- The `---count` formula now counts the number of the visible rows of the column, also non numeric.
- Added the shortcut `Meta + Ctrl + Shift + F` to focus the filter field in the grid.
- Minor graphic improvements.
- Bugs fixing: when a filter was activated or a row deleted, the result of the formulas of the current table was not updated.

**Version 1.3.11**

- Added the shortcut `Meta + Shift + K` to show the citation taken from the bibliographic table that matches the current key.

**Version 1.3.10**

- Added the filter box under the grid.
- The expanded editor uses now the default font of the text with a more readable size.

**Version 1.3.9**

- Added `Meta + Shift + P` to show the picture contained in a Markdown image link at the beginning of the current paragraph.
- Bugs fixing: deleting some cells in the grid didn’t activated the save function.

**Version 1.3.8**

- Bugs fixing: the checking of duplicate words didn’t always work as expected.
- Added the shortcut `Meta + Shift + G` in the grid to search upward.

**Version 1.3.7**

- Added the possibility to assign a todo item to one or more resources and to filter the items assigned to a resource in the todo form.
- Bugs fixing: in the todo form, the heading of the items without a deadline was not reported.
- Bugs fixing: the find and replace functions could be activated also if the search field was empty.
- Bugs fixing: the checking of duplicate words didn’t always work as expected.
- Minor bugs fixing.

**Version 1.3.6**

- Added the button `Replace and find` in the search form.
- Corrected and improved the documentation on bibliography manager.

**Version 1.3.5**

- The `Meta + Opt + C` shortcut now requires that the author(s), the title and the details of a citations are separated by a tab.
- When a link to a picture is created, it’s now properly formatted.

**Version 1.3.4**

- During presentations, if it’s highlighted a link to a picture at the beginning of a paragraph, this picture is shown.
- The shortcut `Meta + Opt + C` now allows to copy more citations divided in different paragraphs.
- The title of the citations is not italicized by default any more (citations of article don’t require that), but the shortcut `Meta + Opt + C` adds the italics markers to the titles if they don’t begin and end with quotation marks.
- Improved the bibliography compilation.
- Bugs fixing: the footnote numbering could not work as expected.
- Minor improvements.

**Version 1.3.3**

- In the document, added the shortcut `Meta + Opt + C` to copy a selected citation so that it can be properly pasted in the bibliographic table.
- In the grid, added the `Meta` + `Ctrl` + `K` to make unique the current key in the second column.

**Version 1.3.2**

- In the grid, added the shortcut `Meta + Opt + C` to copy the bibliographic key in the clipboard among curly brackets.
- Bugs fixing: the bibliography manager didn’t work as expected.
- Bugs fixing: the file search form was not available.

**Version 1.3.1**

- Added the menu item `File - Import tables...`.
- Small bugs fixing.
- Corrected and improved the documentation.

**Version 1.3.0**

- Added bibliographic management (see above form information).
- Added the extended edito in the grid, activated by `Ctrl + Space`.
- Bugs fixing: creating a new footnote when there were some lines after the footnotes block messed the text.

[...]
