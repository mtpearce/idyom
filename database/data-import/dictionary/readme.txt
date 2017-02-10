*** README for database/data-import/dictionary/ ***

This folder contains dictionaries for use in importing certain types of musical file. These dictionaries contain mappings from notated symbols to particular types of musical concept. For example, the pitch-class set (0 3 7) can be denoted by the scale degrees (1 b3 5), the term "minor", or the symbol "m".

Dictionary files should be laid out as follows. They should be comma-separated values (csv) files: that is, each line should correspond to a number of fields, which are separated (delimited) by commas. Fields may additionally be/Users/peter/quicklisp/local-projects/idyom/database/data-import/dictionary/readme.txt surrounded (quoted) with double quotes. This quoting is compulsory when the field itself contains a comma.

These are examples of three types of legal rows:

0 3 7,1 b3 5,minor,m
"0 3 7","1 b3 5","minor","m"
0 4 6 10,1 3 b5 b7,"seventh, flat fifth",7b5

Files should not contain headers; if desired, column information can be added to this readme file. 

These types of files can be created using standard spreadsheet software (e.g. Microsoft Excel) and statistical software (e.g. R), as well as standard text editors (e.g. Notepad, TextEdit, Emacs).

Each row of the file should correspond to a separate musical concept, such as a type of chord. The first element of each row should correspond to a representation of the musical concept in the desired canonic form that IDyOM will use. For example, a chord dictionary might use the ordered pitch-class set as the canonic form. Subsequent elements in the same row should correspond to different symbols that map to that canonic form. In the example given above, the symbols "1 b3 5", "minor", and "m" would all map to "0 3 7". A line representing this information would look like follows:

0 3 7,1 b3 5,minor,m

It is permitted for different rows to have different numbers of elements. 

Sometimes we may wish to use the empty string ("") as a signifier. This cannot be encoded as-is under the present system, because the empty string will be indistinguishable from the empty fields that may result when different rows have different numbers of column entries. We therefore use "{empty-string}" (without quotes) in place of an actual empty string.

Sometimes users may try to import a score into IDyOM and receive an error saying that a certain symbol has not been found. In that case, we advise that you edit the dictionary file yourself and add this symbol to the end of the appropriate row. We would appreciate receiving any edits you make to the dictionary files so that we can incorporate these into future software releases.

The rest of this readme file now describes individual dictionaries.

*** Chord qualities (chord-qualities.csv)

This dictionary describes different chord "qualities": those aspects of the chord that are independent of its root or tonal context. The canonic representation used is the ordered pitch-class set. The second column provides a scale-degree representation of the chord quality, the third column a textual representation, and subsequent columns provide abbreviated representations.

Symbols were primarily derived from the Hal Leonard Pocket Piano Chord Dictionary (author: Andrew DuBrock; publisher: Hal Leonard; city: Cheltenham, Victoria, Australia; published year: 2009). Degree signs for diminished chords were replaced with the text "dim". The scale degrees and pitch classes of the diminished seventh chord were corrected from the original, which instead gave the scale degrees for the half-diminished seventh chord. Additionally, the chord degrees for the "seventh, flat ninth, sharp fifth" were modified to incorporate a flat ninth.

Additional symbols were then added with reference to the McGill Billboard corpus and the following paper: Harte, C., Sandler, M., Abdallah, S. & Gómez, E. Symbolic representation of musical chords: A proposed syntax for text annotations. in Proceedings of the International Conference on Music Information Retrieval 66–71 (2005).