
================================================================
Sha2SML
================================================================

Sha2SML (which is abbreviated for [SHA2 - StandardML]) is
a SHA2 algorithm implementation of StandardML.
SHA2 is defined in rfc6234_.

Feature
================================================================

This library provides below 4 kinds of hash algorithms defined as SHA2:

- SHA224
- SHA256
- SHA384
- SHA512


Environment
================================================================

This library had been developped with SML/NJ **110.82**,
but recent versions should be work well.


Install
================================================================

SML/NJ
----------------------------------------------------------------

To install **Sha2SML** to your environment, run the :code:`install` target of Makefile.smlnj.

.. code-block:: sh

   $ make -f Makefile.smlnj install


Or you can specify install directory with PREFIX like below

.. code-block:: sh

    $ make -f Makefile.smlnj install PREFIX=~/.sml/smlnj


The :code:`install` target requires `SMLDoc`_ to generates documentations.
To install library without documentations, use :code:`install-nodoc` target.


.. code-block:: sh

    $ make -f Makefile.smlnj install-nodoc


To complete the installation, add an entry to the *PATHCONFIG* file.

.. code-block:: sh

    $ echo "libsha2sml .sml/smlnj/lib/libsha2sml" >> ~/.smlnj-pathconfig


Once the above steps are completed, Sha2SML can be used from anywhere in your environment:

.. code-block:: sml

    $ sml
    > CM.make "$/libsha2sml.cm";
    (* ... snip ... *)
    [New bindings added.]
    val it = true : bool


Load to Interactive Environment
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sha2SML allows users to loading it in place:

.. code-block:: sml

    $ sml
    - CM.make "libsha2sml.cm";
    (* ... snip ... *)
    val it = true : bool
    - Sha256.hashString "";
    val it = - : ?.Sha256.word Sha256.t
    - Sha256.toString it;
    val it = "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"
      : string


MLton
----------------------------------------------------------------

To install **Sha2SML** for MLton, run the :code:`install` target of Makefile.mlton.

.. code-block:: sh

    $ make -f Makefile.mlton install


Or you can specify install directory with PREFIX like below

.. code-block:: sh

    $ make -f Makefile.mlton install PREFIX=~/.sml/mlton


The :code:`install` target requires `SMLDoc`_ to generates documentations.
To install library without documentations, specify :code:`install-nodoc` target.

.. code-block:: sh

    $ make -f Makefile.mlton install-nodoc


To complete the installation, add an entry to the *mlb-path-map* file as follows.

.. code-block:: sh

    $ PREFIX=... # default /usr/local/mlton
    $ make install PREFIX=${PREFIX}
    $ echo "SHA2SML ${PREFIX}/lib/libsha2sml" >> /path/to/mlb-path-map


Poly/ML
----------------------------------------------------------------

Compile the module with default target.

.. code-block:: sh

    $ make -f Makefile.polyml


The default target generates `libsha2sml-x.y.z.poly` in this directory.
To install this library, use `install` target:

.. code-block:: sh

    $ make -f Makefile.polyml install


To change the installation directory, specify `PREFIX` variable like:

.. code-block:: sh

    $ make -f Makefile.polyml PREFIX=~/.sml install



Load to Interactive Environment
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sha2SML allows users to loading it in place:

.. code-block:: sml

    $ poly
    - PolyML.loadModule "./libsha2sml.poly";
    (* ... snip ... *)
    val it = (): unit
    - Sha256.hashString "";
    val it = - : ?.Sha256.word Sha256.t
    - Sha256.toString it;
    val it = "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855":
      string



Test
================================================================

This project containts a unit test module provides many test cases.
These test cases are imported from:

- `Cryptographic Standards and Guidelines`_
- `NESSIE test vectors`_
- `Secure Hash Standard Validation System (SHAVS)`_

.. Note:: Not all cases are executed as unit test.


SML/NJ
----------------------------------------------------------------

To run the unit tests, run the :code:`test` target.

.. code-block:: sh

    $ make -f Makefile.smlnj test
    (* ... snip ... *)
    Heap was already up-to-date.
    .......................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
    tests = 951, failures = 0, errors = 0
    Failures:
    Errors:


MLton
----------------------------------------------------------------

To run the unit tests, run the :code:`test` target.

.. code-block:: sh

    $ make -f Makefile.mlton test


    $ ./test/sources
    .......................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
    tests = 951, failures = 0, errors = 0
    Failures:
    Errors:


Poly/ML
----------------------------------------------------------------

Building and executing the unit test project with Make.

.. code-block:: sh

    $ export LIBSMLUNIT=~/path/to/libsmlunit.poly
    $ make -f Makefile.polyml test
    Making test
    Making Sha2Test
    ..
    polyc -o sha2test-poly sha2test-poly.o
    ./sha2test-poly
    .......................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
    tests = 951, failures = 0, errors = 0
    Failures:
    Errors:



.. _rfc6234: https://tools.ietf.org/html/rfc6234
.. _`Cryptographic Standards and Guidelines`: https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values
.. _`NESSIE test vectors`: https://www.cosic.esat.kuleuven.be/nessie/testvectors/hash/sha/
.. _`Secure Hash Standard Validation System (SHAVS)`: https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing#shavs

.. _`SMLDoc`: https://www.pllab.riec.tohoku.ac.jp/smlsharp//?SMLDoc
