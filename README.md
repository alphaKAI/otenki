# otenki

[tenki](https://github.com/algon-320/tenki)コマンドがPython2で書かれていて，ちょっと古いな〜と思い，OCamlで書き直してみたもの．

## Prerequirements

* OPAM 2
* OCaml 4.07
* dune

## Installation

otenkiはduneでビルドします．  
duneはopamで`opam install dune`でインストールできます．  
そこまでくれば，

```sh
$ opam install core ppx_deriving ppx_jane sexplib camomile lambdasoup conf-gnutls ocamlnet re
```
で依存ライブラリがインストールされ，さらに

```sh
$ dune build
```

とすれば依存ライブラリがインストールされたうえで，ビルドすることができ，

```sh
$ dune exec otenki
```

で実行することができます．  
オプションの指定については

```sh
$ dune exec otenki -- -help
```
とすることで表示されます．  

## LICENSE
otenkiはMITライセンスのもとで配布します．詳細はLICENSEファイルを見てください．  
Copyright (C) 2018 Akihiro Shoji  