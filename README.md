# hydrant

![570502](https://user-images.githubusercontent.com/355756/32174474-0fa70198-bdd6-11e7-976a-d8aaabc437c9.jpg)

Hydrant is a Haskell library for fast and simple markup construction.

It is a library that turns trees into strings. It aims to do that
quickly and without a lot of user effort. It aspires to have a very
limited role in your life.

Under the hood, Hydrant is nothing but newtypes and text builders.
It leans heavily on the wonderful `text` library. This simplicity
and the wonderful engineering effort put into `text` make the naive
generation of markup quite fast at runtime.
