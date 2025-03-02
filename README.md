Emacs Lisp port of [Mini Ruccola (vm2gol-v2)](https://github.com/sonota88/vm2gol-v2) compiler

---

```
  $ emacs --version | head -1
GNU Emacs 29.3
```

```
git clone --recursive https://github.com/sonota88/mini-ruccola-emacs-lisp.git
cd mini-ruccola-emacs-lisp

./docker.sh build
./test.sh all
```

```
  $ LANG=C wc -l mrcl_{lexer,parser,codegen}.el lib/*.el
   57 mrcl_lexer.el
  269 mrcl_parser.el
  257 mrcl_codegen.el
   43 lib/json.el
   60 lib/utils.el
  686 total

  # main part
  $ LANG=C wc -l mrcl_{lexer,parser,codegen}.el
   57 mrcl_lexer.el
  269 mrcl_parser.el
  257 mrcl_codegen.el
  583 total
```
