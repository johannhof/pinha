Pinha
=================

This is my experimental Erlang implementation of the Angry-Kings Server ([https://github.com/johannhof/angry-kings-server](https://github.com/johannhof/angry-kings-server)).

The goal is to provide more stability and less side effects than the Node implementation, while preserving the speed and ease of development of the original.

I have found both Erlang and Node excellent platforms to develop realtime game servers on.

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/pinha console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).
