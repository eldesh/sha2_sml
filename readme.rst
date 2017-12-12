
Sha2SML
================================================================

Sha2SML (which is abbreviated for [SHA2 - StandardML]) is
a SHA2 algorithm implementation of StandardML.
SHA2 is defined in rfc6234_.

Feature
----------------------------------------------------------------

This library provides below 4 kinds of hash algorithms defined as SHA2:

- SHA224
- SHA256
- SHA384
- SHA512


Environment
----------------------------------------------------------------

This library had been developped with SML/NJ **110.82**,
but recent versions should be work well.


Load to Interactive Environment
----------------------------------------------------------------

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


Install
----------------------------------------------------------------

For install **Sha2SML** as a SML library to your environment,
stabilize build then copy the produced binaries.

.. code-block:: sh

    $ DESTDIR=${HOME}/.smlnj/lib
    $ echo 'CM.stabilize true "libsha2sml.cm";' | sml
    (* ... snip ... *)
    val it = true : bool
    $ echo "libsha2sml.cm ${DESTDIR}/libsha2sml.cm" >> ~/.smlnj-pathconfig
    $ mkdir -p ${DESTDIR}/libsha2sml.cm
    $ cp -R .cm ${DESTDIR}/libsha2sml.cm/.cm

When above steps have completed, Sha2SML can be used from anywhere in your environment:

.. code-block:: sml

    $ sml
    > CM.make "$/libsha2sml.cm";
    (* ... snip ... *)
    [New bindings added.]
    val it = true : bool


Test
----------------------------------------------------------------

This project containts a unit test module provides many test cases.
These test cases are imported from:

- `Cryptographic Standards and Guidelines`_
- `NESSIE test vectors`_
- `Secure Hash Standard Validation System (SHAVS)`_

.. Note:: Not all cases are executed as unit test.


Execute Unit Test
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Loading the unit test CM file, some test cases would be executed:

.. code-block:: sh

    $ echo "" | sml -m test/sources.cm

Then you will see the result of each test case.

.. code-block:: sh

    ...................................................................................................................................................................................
    tests = 179, failures = 0, errors = 0
    Failures:
    Errors:



.. _rfc6234: https://tools.ietf.org/html/rfc6234
.. _`Cryptographic Standards and Guidelines`: https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values
.. _`NESSIE test vectors`: https://www.cosic.esat.kuleuven.be/nessie/testvectors/hash/sha/
.. _`Secure Hash Standard Validation System (SHAVS)`: https://csrc.nist.gov/Projects/Cryptographic-Algorithm-Validation-Program/Secure-Hashing#shavs

