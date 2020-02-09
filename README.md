# Symbiote

Iteration of the [Symbiotic-Data Test Suite](https://docs.symbiotic-data.io/en/latest/testsuite.html).

This project aims to be a network agnostic and data format agnostic serialization verifier - it's main
purpose is to verify that _data_, _operations_ on that data, and _serialization_ of that data is
all consistent for multiple platforms. This project defines a trivial protocol that can be re-implemented
in any platform, over any network medium.

## How the Protocol Works

It's a pretty simple idea:

![](https://github.com/athanclark/symbiote/raw/master/images/drawing-rendered.png)

### Setting

Basically, the test suite envolves two peers - we'll call them Peer A and Peer B, and they're connected
by some network. Likewise, they are both using (possibly different) systems, which we've denoted as Lang A
(like Haskell) and Lang B (like PureScript / JavaScript).

The goal of this suite is to ensure that the _same idea_ defined on these different platforms have the
_same representation_ - both operationally (i.e., QuickCheck laws) and through some encoding (like printing
the data to JSON or a ByteString).

In the diagram above, each system has some idea of what "Type T" looks and feels like, but could be completely
different implementations on each system. Again, though, we care about their _outcome_ being identical; so we
disambiguate the idea from the data.

### Protocol

The first step in the protocol is for the generating party (Peer A) to generate an instance of Type T, and an operation
on that type - this operation, too, needs a serialization implementation (but for most intents and purposes,
the implementation will be only for the scope of use with this test suite).

Peer A then does two things: it serializes both the operation and instance to the network-acceptable format, _and_
it "performs" the operation on that instance to get an _expected result_. Basically, Peer A is using Peer B as a
remote procedure call for that operation, and verifying that the result obtained remotely is identical to the
result obtained locally.

### Remote Procedure Call

As just stated, Peer A then sends the serialized instance and operation to Peer B over that network, where Peer B
decodes the data, and performs the operation. Peer B then encodes the result, and sends it back to Peer A.

### Verification

Peer A now receives the performed data from Peer B, and can now decode it, and test to see if it is equal to the
expected result. If it is, it will tell Peer B that it's "their turn" to generate data.


All of this relies on QuickCheck to generate the data - with the size of the generated data increasing at each step,
until a maximum is reached (which is clarified in the documentation).

-------------

For an example, check out the tests - they are performed both locally in an asynchronous channel, and remotely
with its sister package - [purescript-symbiote](https://pursuit.purescript.org/package/purescript-symbiote).
