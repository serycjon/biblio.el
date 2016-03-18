# `biblio.el`: An Emacs package for browsing and fetching references

`biblio.el` makes it easy to browse and gather bibliographic references and
publications from various sources, by keywords or by DOI.  References are
automatically fetched from well-curated sources, and formatted as BibTeX.

## Supported sources:

* `dx.doi.org` (to retrieve BibTeX records from DOIs)
* `CrossRef` (for general searches)
* `DBLP` (specialized in computer science)
* `Dissemin` (to gather information about a particular publication, such as its
  open acces status)

## Usage

Each source can be accessed independently:

* `M-x crossref-lookup` to query CrossRef
* `M-x dblp-lookup` to query DBLP
* `M-x doi-insert` to insert a BibTeX record by DOI
* `M-x dissemin-lookup` to show information about the open access status a
  particular DOI

Most of these commands work together: for example, `crossref-lookup` displays a
list of results in `biblio-selection-mode`.  In that mode, use:

* `c` or `M-w` to copy a BibTeX record for the current entry
* `i` or `C-y` to insert a BibTex record for the current entry in source buffer.
* `o` to run an extended action, such as fetching a Dissemin record.

## Examples

* To insert a clean BibTeX entry for [this paper](http://dx.doi.org/10.1145/2676726.2677006) in the current buffer, use
    ```
    M-x crossref-lookup RET fiat deductive delaware RET i
    ```
    (the last `i` inserts a BibTeX record for the currently selected entry in your buffer).

* To find publications by computer scientist Leslie Lamport, use `M-x dblp-lookup RET author:Lamport RET`.

* To check whether an article is available online, use `o` in the list of results.
