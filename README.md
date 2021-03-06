Odefa
=====

This directory contains an implementation of the language discussed in the paper
"Higher-Order Demand-Driven Program Analysis".  This document contains
information about compiling and running the Odefa toploop as well as information
about the contents of this directory.

Pre-generated Results
---------------------

If readers wish to explore output without compiling the application, they are
referred to the `sample-output` directory.  This directory contains a series of
DOT and PDF files.  Each file contains either a DDPA or PDS graph for one of the
Overview examples at some level of polyvariance; the particular case is
specified by the filename.  Please note that all output in this directory was
generated automatically by the Odefa toploop.

Compilation
-----------

There are three different ways to setup and run Odefa.

### OPAM

1. Make sure you have [OCaml][ocaml] and [OPAM][opam] installed on the latest
   version:

    $ opam init  # necessary for freshly-installed OPAM instances
    $ eval `opam config env`  # if you do not have OPAM's environment configured
    $ opam update
    $ opam upgrade
    $ opam switch 4.02.2  # this may take a while

2. Install the dependencies:

    $ opam install oasis batteries menhir ounit ppx_deriving
   
   If your shell hashes binary locations, you may need to clear your hashes now.
   (In bash, `hash -r` does this.)

3. Generate configuration:

    $ oasis setup -setup-update dynamic

4. Configure:

    $ ./configure

5. Enable tests:

    $ ocaml setup.ml -configure --enable-tests

6. Build:

    $ make

7. Interact with the toploop (sample programs can be found at `test-sources/`):

    $ ocamlrun odefa_toploop.byte

8. Run the tests:

    $ make test

### Docker

Having [Docker][docker] and [Docker Compose][docker-compose] installed, run:

    $ docker-compose run --rm odefa

This builds and runs the tests.

In order to interact with the toploop (sample programs can be found at
`test-sources/`):

    $ docker-compose run --rm odefa 'ocamlrun odefa_toploop.byte'
    
### Vagrant

Having [VirtualBox][virtual-box] and [Vagrant][vagrant] installed, run:

    $ vagrant up && vagrant exec docker-compose run --rm odefa

This builds and runs the tests.

In order to interact with the toploop (sample programs can be found at
`test-sources/`):

    $ vagrant exec docker-compose run --rm odefa 'ocamlrun odefa_toploop.byte'
    
Execution
---------

The Odefa toploop accepts command-line arguments.  Brief help for these
arguments may be obtained by passing `--help`.  Notable options are:

### `--log=debug`

Enables debug logging.
   
### `--analysis=0`

Uses a monovariant analysis, disabling context-sensitivity.

### `--analysis=1`

Uses the analysis described in the paper (default).

### `--analysis=2`

Uses a 2-level context stack.  This is necessary to get ideal precision on the
identity example in the paper, as it nests a call for illustration.

### `--dotgen`

Generates Graphviz DOT files of the DDPA and PDS graphs.  These files may be
processed by Graphviz as follows:

    dot -Tpdf < odefa-ddpa-graph.dot > odefa-ddpa-graph.pdf
                    
Authors
-------

- Leandro Facchinetti <lfacchi2@jhu.edu>.
- Zachary Palmer <zachary.palmer@jhu.edu>.
- Scott F. Smith <scott@jhu.edu>.

The Johns Hopkins University


[ocaml]: https://ocaml.org/
[opam]: https://opam.ocaml.org/
[docker]: https://www.docker.com/
[docker-compose]: https://docs.docker.com/compose/
