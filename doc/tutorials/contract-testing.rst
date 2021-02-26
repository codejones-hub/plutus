.. highlight:: haskell
.. _contract_testing_tutorial:

Property-based testing of Plutus contracts
==========================================

Plutus comes with a library for testing contracts using
QuickCheck. Tests perform a sequence of calls to contract endpoints,
checking that tokens end up in the correct wallets at the end of each
test. These sequences can be generated at random, or in a more
directed way to check that desirable states always remain
reachable. This tutorial introduces the testing library by walking
through a simple example: a contract that implements a guessing game.

An overview of the guessing game
--------------------------------

The source code of the guessing game contract is provided as an
example here_. The game is played as follows:

.. _here: http://no.it.isnt/

 - The first player creates an instance of the contract, holding a sum
   of ADA that the player donates as a prize. The prize is protected
   by a secret password.

 - Any player can now try to guess the password, by submitting a
   'guess' transaction that attempts to withdraw some or all of the
   prize, along with a guess at the password, and a new password to
   replace the old one. If the guess is correct, and the contract
   contains enough ADA, then the guesser receives the withdrawal and
   the remainder of the prize is now protected by the new password. If
   the guess is wrong, the transaction is not accepted, and nothing
   changes. 

As an extra wrinkle, when the first player creates the contract, a new
token is also forged (of a new token type [#]_). Only the player
currently holding the token is allowed to make a guess--which gives us
an opportunity to illustrate forging and passing around tokens.

.. [#] Ulf, is this true?

Emulated wallets
----------------

To test contracts, we need emulated wallets. These and many other
useful definitions for testing can be imported via

.. literalinclude:: GameModel.hs
    :start-after: START_IMPORT_CONTRACT_TEST
    :end-before: END_IMPORT_CONTRACT_TEST

Now we can create a number of wallets: in this
tutorial, we'll settle for three:

.. literalinclude:: GameModel.hs
    :start-after: START_DEFINE_WALLETS
    :end-before:  END_DEFINE_WALLETS

                 
Introducing contract models
---------------------------

We test contracts using a *model* of the contract state; the first job
to be done is thus defining that model. To do so, we import the
contract modelling library

.. literalinclude:: GameModel.hs
    :start-after: START_IMPORT_CONTRACT_MODEL
    :end-before: END_IMPORT_CONTRACT_MODEL

and define the model type:

.. literalinclude:: GameModel.hs
    :start-after: START_MODEL_TYPE
    :end-before: END_MODEL_TYPE

This definition is incomplete: we shall fill in further details as we proceed.

The ``GameModel`` type must be an instance of the ContractModel_
class, which has an associated datatype defining the kinds of
*actions* that will be performed in generated tests.

.. literalinclude:: GameModel.hs
    :start-after: START_INSTANCE
    :end-before: END_INSTANCE

In this case we define three actions:

 - a ``Lock`` action to be performed by the first player to create the
   contract, containing the player's wallet (from which the ADA will
   be taken), the secret password, and the prize amount.
 - a ``Guess`` action to be performed by the other players, containing
   the player's wallet (to receive the prize), the player's guess, a
   new password, and the amount to be claimed if the guess is right.
 - a ``GiveToken`` action, to give the game token to a player so they
   can make a guess.

A generated test is called a ``Script``, and is (essentially) a
sequence of ``Action``. We can run tests by using `propRunScript_`_:

.. literalinclude:: GameModel.hs
    :start-after: START_GAME_PROPERTY
    :end-before: END_GAME_PROPERTY

When we test this property, ``quickCheck`` will generated random
scripts to be tested. But what is the ``handleSpec``?
`propRunScript_`_ needs to know which contract handles it should create
for use in this test, and the ``handleSpec`` tells it. Every handle is
*named* by a ``HandleKey``, which is another datatype associated with
the ContractModel_ class. It needs to be defined as a GADT, because it
also defines the types of the associated contract schema and contract
errors:

.. literalinclude:: GameModel.hs
    :start-after: START_HANDLE_KEY
    :end-before: END_HANDLE_KEY

***I don't really understand this. Ulf, can you explain here what the
handle key is, and why it contains a wallet? ***

Once the type of ``HandleKey`` is defined, we can construct our
HandleSpec_:

.. literalinclude:: GameModel.hs
    :start-after: START_HANDLE_SPEC
    :end-before: END_HANDLE_SPEC

Here ``G.contract`` is the contract under test. Notice that we can
test several contracts together with one contract model.

Of course, trying to test this property will not yet succeed:

::
   
  | > quickCheck prop_Game
  | \*\*\* Failed! (after 1 test and 1 shrink):
  | Exception:
  |   GSM.hs:65:10-32: No instance nor default method for class operation arbitraryAction

  
The contract modelling library cannot generate test cases, unless *we*
specify how to generate ``Action``, which we will do next.
    
.. _ContractModel: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:ContractModel

.. _propRunScript_: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:propRunScript_

.. _HandleSpec: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:HandleSpec

Generating actions
------------------

To generate actions, we need to be able to generate wallets, guesses,
and suitable values of ADA, since these appear as action parameters.

.. literalinclude:: GameModel.hs
    :start-after: START_GENERATORS
    :end-before: END_GENERATORS

We choose wallets from the three available, and we choose passwords
from a small set, so that relatively many random guesses will be
correct. We choose ADA amounts to be non-negative integers, because
negative amounts would be error cases that we choose not to test.
*** Is this really a good idea? Will a player who accidentally tries
to claim a negative sum actually lose money? ***

Now we can define a generator for ``Action``, as a method of the
ContractModel_ class:

.. literalinclude:: GameModel.hs
    :start-after: START_ARBITRARY
    :end-before: END_ARBITRARY

With this method defined, we can start to generate test cases. Using
``sample`` we can see what scripts look like:

::
  | > sample (arbitrary :: Gen (Script GameModel))
  | Script
  |  [Var 1 := Lock (Wallet {getWallet = 2}) "hunter2" 5,
  |   Var 2 := Guess (Wallet {getWallet = 3}) "*******" "hello" 6,
  |   Var 3 := Guess (Wallet {getWallet = 1}) "secret" "*******" 10,
  |   Var 4 := Guess (Wallet {getWallet = 3}) "*******" "*******" 6,
  |   Var 5 := GiveToken (Wallet {getWallet = 3}),
  |   Var 6 := Guess (Wallet {getWallet = 2}) "hunter2" "hunter2" 15]

and we can even run 'tests', although they don't do much yet:

::
  > quickCheck prop_Game
  +++ OK, passed 100 tests:
  84% contains Lock
  80% contains GiveToken
  79% contains Guess

  Actions (2263 in total):
  33.94% Lock
  33.89% Guess
  32.17% GiveToken

    
Introduce the class (where to import it from). Need a model
state. Minimal class definition to get some tests running (action
type, generate and perform commands).

Form of a script. Running tests.




Introduction to testing and stuff.

A guessing game contract
------------------------

Explain the contract. Double back-ticks for fixed-width font
``Language.PlutusTx.Coordination.Contracts.GameStateMachine``.
Link to other sections :ref:`like this <modelling-contracts>`.

.. _modelling-contracts:

Modelling contracts
-------------------

You can include Haskell code like this:

.. literalinclude:: GameModel.hs
    :start-after: START_MODELSTATE
    :end-before: END_MODELSTATE

Linking to the haddock docs: `arbitraryAction`_

.. note::
    This is a note.

.. _arbitraryAction: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:arbitraryAction

Questions to resolve
--------------------
What happens if we try to withdraw 0 ADA? What happens when the last ADA is withdrawn? Is it possible to delete the contract?
