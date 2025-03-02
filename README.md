Emacs Lisp port of [Mini Ruccola (vm2gol-v2)](https://github.com/sonota88/vm2gol-v2) compiler

[Emacs Lispでシンプルな自作言語のコンパイラを書いた](https://qiita.com/sonota88/items/a8b612e75b8f13d55772)


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
  $ LANG=C wc -l mrcl_{lexer,parser,codegen}.el
   57 mrcl_lexer.el
  269 mrcl_parser.el
  257 mrcl_codegen.el
  583 total
```
