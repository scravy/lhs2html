Literate Haskell to HTML
========================

Install
-------

`cabal install lhs2html`

Usage
-----

    >>> ls src
    Module.lhs YAM.lhs

    >>> lhs2html src
    >>> ls src
    Module.lhs YAM.lhs Module.lhs.htm YAM.lhs.htm

    >>> lhs2html -m src/Module.lhs
    >>> ls src
    Module.lhs YAM.lhs Module.lhs.htm YAM.lhs.htm Module.lhs.md

    >>> lhs2html -h
      -m  --markdown  transform to github flavored markdown (*.md)
      -u  --unlint    plain unlit to *.hs
      -p  --parsetree show parse tree as *.txt
      -h  --help      this help
      -v  --version   show version information


