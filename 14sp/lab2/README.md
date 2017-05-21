% Stanford CS240h Lab 2

You will write a basic chat server. It will support any number of
clients and allow them to join and leave at any time. Please implement
it as follows:

* Clients and the server will communicate over TCP.
* The server should read the `CHAT_SERVER_PORT` environment variable
  which will contain an `Int` and listen on that port for clients.
* Client's should have a username picked for them automatically by the
  server. Usernames should simply be a number, where the server simply
  maintains a counter (starting from 1) for which client this is by
  ordering of connection. I.e., first client to connect gets username
  `1`, second client gets username `2`.
* Clients will simply be telnet, sending UTF-8 encoded text to the
  server.
* A message from a client C should be sent to all other connected
  clients but not echoed back to C. The message M from client C should
  be sent prefixed by `"<C.username>: "`. For example, if client with
  username `"12"` sends the message `"hello world"`, it should be sent
  to all other connected clients as: `"12: hello world"`.
* When a client C with username U joins the server, please broadcast
  this message to all clients (including C): `"U has joined"`
* When a client C with username U quits the server, please broadcast
  this message: `"U has left"`

We are providing a skeleton Cabal project to help get started,
download it from
[here](http://www.scs.stanford.edu/14sp-cs240h/labs/lab2.tar.gz).

## Example Transcript

```
1 has joined
2 has joined
1: hello!
2: hi
3 has joined
3: hi
1 has left
2: hi 3
```

## Bonus

As a bonus, you can implement rooms and private messaging. All clients
should connect initially to a default room so that testing of the
standard assingment still works.

The syntax for changing to a new room is:

```
/join <room name>
```

i.e., `/join haskell` will cause the client to change to the 'haskell'
chat room.

Please ensure the command is valid (i.e., only contains 1 argument),
if it isn't please respond to the client with:

```
server: invalid /join command
```

Rooms should function as if they were independent servers. So when a
user `U` leaves rooms `R1` and joins room `R2`, two things should
happen:

* All users in R1 should receive a "U has left" message
* All users in R2 should receive a "U has joined" message
* U itself should receive receive only the "U has joined" message.

The syntax for messaging is:

```
/msg <username> <message>
```

i.e., `/msg 12 hello world` will send a private message to client 12.

Please ensure the command is valid, if it isn't then respond to the
client with:

```
server: invalid /msg command
```

Finally, you don't need to worry that a client can no longer send a
message `"/msg"` or `"/join"` to everyone, that will simply be a
limitation.

## Due Date

Lab 2 should be submitted by the start of class (12:50pm) on *Tuesday,
April 22nd*.

You have 48 hours of late days for the three labs. They are consumed
in 24 hour blocks and are used automatically. After they are used,
you'll have the maximum grade you can receive for a late lab reduced
by 25% each day.

## Cabal -- Build & Test Tool

Cabal is the standard build and packaging tool for Haskell. A starting
framework is provided for you. You can find the user guide for Cabal
[here](http://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites).

## Provided Files

The files provided to get started are:

* chat.cabal -- specifies the build system.
* Server.hs -- main module, simply runs the chat server.
* Chat.hs -- you should edit this file to implement your chat server.
* TestChat.hs -- the test harness. You need to edit this and add your
  own tests!

If you add any new files, please make sure to add them into
`chat.cabal` as otherwise they won't be packaged up when you run
`cabal sdist` and then they won't be sumitted.

## Building Lab 2

To get up and running (using Cabal), issue the following commands:

        cabal sandbox init

This will initiate a self-contained build environment where any
dependencies you need are installed locally in the current directory.
This helps avoid the Haskell equivalent of 'DLL Hell!'. If your
version of cabal is older such that it doesn't have the `sandbox`
command, then just proceed without it and it should all be fine.

Next, you want to build the lab. For that, issue the following
commands:

        cabal install --only-dependencies --enable-tests
        cabal configure --enable-tests
        cabal build

After that, you should also be able to run the test harness simply by
typing:

        cabal test

And you'll get some pretty output!

## Testing Lab 2

Some skeleton code for a test framework is provided in
`TestHarness.hs`. You'll need to edit it to add your own tests. The
test framework uses a Haskell package called
[hspec](http://hspec.github.io/). Please refer to it for documentation
on how to use it.

We also strongly encourage the use of [quick
check](http://hackage.haskell.org/package/QuickCheck)! It can be
integrated with the test driver that HSpec provides, please see the
skeleton code that has an example of how.

## Grading

While we strongly encourage you to take testing seriously and write a
comprehensive test suite, we are only going to grade you on your glob
matching implementation.

Grading will be just done on functionality but we will try to give
feedback on your coding style.

## Submitting

First, simply type:

        cabal sdist

This will generate a tar file of your code in `dist/globber.tar.gz`.

Then go to [upload.ghc.io](http://upload.ghc.io/) and submit your work
through the online form. You can resubmit as many times as you want up
until the deadline.

If you have any trouble submitting on-line, then please email the
staff mailing [list](mailto:cs240h-staff@scs.stanford.edu).

