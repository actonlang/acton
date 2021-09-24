# Install Acton from a GitHub release tar ball

There are pre-built binary releases available from GitHub Releases.  Download a
tar ball from [the Release page](https://github.com/actonlang/acton/releases).
Pick the latest stable versioned release.

In case you are looking to live on the bleeding edge or have been asked by a
developer (in case you ran into a bug) you can pick `tip`, which is built
directly from the `main` branch.

Extract the Acton tar ball:
```
$ tar jxvf acton-*
```

You will want to include the `acton/bin` directory in your `PATH` so you can use
`actonc`.

`actonc` has run time dependencies and you will need to install the necessary
dependencies for your platform.

#### Debian
```
apt install gcc libprotobuf-c-dev libutf8proc-dev
```

#### Mac OS X
```
brew install protobuf-c util-linux
```

