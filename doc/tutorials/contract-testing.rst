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

.. code-block:: haskell
                
   data GameModel = GameModel

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

.. code-block:: text
                
  > quickCheck prop_Game
  *** Failed! (after 1 test and 1 shrink):
  Exception:
    GSM.hs:65:10-32: No instance nor default method for class operation arbitraryAction
                 
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
from a small set, so that random guesses will often be
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

.. code-block:: text

  > sample (arbitrary :: Gen (Script GameModel))
  Script
    [Var 1 := Lock (Wallet {getWallet = 2}) "hunter2" 5,
     Var 2 := Guess (Wallet {getWallet = 3}) "*******" "hello" 6,
     Var 3 := Guess (Wallet {getWallet = 1}) "secret" "*******" 10,
     Var 4 := Guess (Wallet {getWallet = 3}) "*******" "*******" 6,
     Var 5 := GiveToken (Wallet {getWallet = 3}),
     Var 6 := Guess (Wallet {getWallet = 2}) "hunter2" "hunter2" 15]
  .
  .

(The ``Var 1``-to-``Var 6`` here are 'variables' bound to the result of each call--in this case, nothing interesting). *Will the variables be useful at any point in the future? If not, they just look a distraction, and maybe we should remove them.*


We can even run 'tests' now, although they don't do much yet:

.. code-block:: text
                
  > quickCheck prop_Game
  +++ OK, passed 100 tests:
  84% contains Lock
  80% contains GiveToken
  79% contains Guess
  
  Actions (2263 in total):
  33.94% Lock
  33.89% Guess
  32.17% GiveToken

Notice that the output contains two tables. The first one just tells
us that 84 of the 100 generated test sequences contained a ``Lock``,
80 contained a ``GiveToken`` and so on. **Maybe we should remove the first table. It's rarely interesting.** The second is more
interesting: it tells us the distribution of generated Actions,
aggregated across all the tests. We can see that each action was
generated around one third of the time, which is to be expected since
our generator does not weight them at all. Keep an eye on this table
as we extend our generation; if any ``Action`` disappears altogether,
or is generated very rarely, then this indicates a problem in our
tests.

Modelling expectations
----------------------

The ultimate purpose of our tests is to check that funds are
transferred correctly by each operation--for example, that after a
guess, the guesser receives the requested ADA only if the guess was
correct. An important part of a ContractModel_ defines how funds
are expected to move. However, it's clear that in order to define how
we expect funds to move after a ``Guess``, we need to know more than
just where all the ADA are. We need to know:

- what the current secret password is, so we can decide whether or
  not the guess is correct.

- whether or not the guesser currently holds the game token, and so is
  entitled to make a guess.

- how much ADA is currently locked in the contract, so we can
  determine whether the guesser is requesting funds that actually
  exist.

These all depend on the previous steps in the test case. To keep track
of such information, we store it in a *contract state*, which is the
type parameter of the ContractModel_ class. So, let's complete the
definition of a ``GameModel``:

.. literalinclude:: GameModel.hs
   :start-after:  START_MODELSTATE
   :end-before:   END_MODELSTATE

Initially the game token does not exist, so we record its current
owner as a ``Maybe Wallet``, so that we can represent the initial
situation before its creation.

Now we can define the initial state of the model at the start of each
test case, initialState_, and a nextState_ function to model the way
we expect each operation to change the state. These are both methods
in the ContractModel_ class.

The initial state just records that the game token does not exist yet,
and assigns default values to the other fields.

.. literalinclude:: GameModel.hs
   :start-after:  START_INITSTATEDEFS
   :end-before:   END_INITSTATEDEFS

The nextState_ function is defined in the Spec_ monad

.. code-block:: haskell

   nextState :: Action state -> Spec state ()

and defines the expected effect of each operation.

Creating the contract initializes the model state (using `($=)`_
and generated ``Lens`` operations), forges the game token (using
forge_), deposits it in the creator's wallet, and withdraws the ADA
locked in the contract (using deposit_ and withdraw_):

.. code-block:: haskell

    nextState (Lock w secret val) = do
        hasToken      $= Just w
        currentSecret $= secret
        gameValue     $= val
        forge gameTokenVal
        deposit  w gameTokenVal
        withdraw w $ Ada.lovelaceValueOf val

When making a guess, we need to check parts of the contract state
(which we read using viewContractState_), and then we update the
stored password, game value, and wallet contents appropriately. (Here
`($~)`_ applies a function to modify a field of the contract state).


.. code-block:: haskell
                
    nextState (Guess w old new val) = do
        correctGuess <- (old ==)    <$> viewContractState currentSecret
        holdsToken   <- (Just w ==) <$> viewContractState hasToken
        enough       <- (>= val)    <$> viewContractState gameValue
        when (correctGuess && holdsToken && enough) $ do
            currentSecret $= new
            gameValue     $~ subtract val
            deposit w $ Ada.lovelaceValueOf val

 - ``GiveToken`` just transfers the game token from one wallet to another using transfer_.

.. code-block:: haskell

    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewContractState hasToken
        transfer w0 w gameTokenVal
        hasToken $= Just w

At the end of each test, the ContractModel_ framework checks that
every wallet contains the tokens that the model says it should.

The code above refers to the game token, ``gameTokenVal``, and of course we
have to define this. A suitable definition is

.. literalinclude:: GameModel.hs
   :start-after:  START_GAMETOKEN
   :end-before:   END_GAMETOKEN

*** Can you explain this code, Ulf? ***

We need to make the following imports also:

.. literalinclude:: GameModel.hs
   :start-after: START_NEXTSTATEIMPORTS
   :end-before:  END_NEXTSTATEIMPORTS

We can exercise the nextState_ function already by generating and
'running' tests, even though we have not yet connected these tests to
the real contract. Doing so immediately reveals a problem:

.. code-block:: text
                
  > quickCheck prop_Game
  *** Failed! (after 1 test):
  Exception:
    Maybe.fromJust: Nothing
    CallStack (from HasCallStack):
      error, called at libraries/base/Data/Maybe.hs:148:21 in base:Data.Maybe
      fromJust, called at GSM0.hs:122:15 in main:GSM0
  Script
   [Var 1 := GiveToken (Wallet {getWallet = 2})]

Looking at the last two lines, we see the generated test script, and
the problem is evident: we generated a test *that only gives the game
token* to wallet 2, but this makes no sense because the game token has
not yet been forged--so the ``fromJust`` in the nextState_ function
fails. We will see how to prevent this in the next section.

Performing actions
------------------

So far we are generating Actions, but we have not yet linked them to
the contract they are supposed to test--so 'running' the tests, as we
did above, did not invoke the contract at all. 
 
Introduce the class (where to import it from). Need a model
state. Minimal class definition to get some tests running (action
type, generate and perform commands).

Need to introduce:
  - performing actions
    GiveToken needs to know where the token is
    State needs to be introduced, initialState, nextState
    precondition to force token to be forged first
  - defining the token type

Defining 'expected behaviour'... how funds move. Need to know:
 - is the guess correct?
 - does the guesser have the token?
 - how much is in the contract right now?

   

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
.. _nextState: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:nextState
.. _initialState: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:initialState
.. _Spec: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#g:3
.. _`($=)`: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:`($=)`
.. _`($~)`: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:`($~)`

.. _forge: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:forge
.. _deposit: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:deposit
.. _withdraw: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:withdraw
.. _transfer: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:transfer
.. _viewContractState: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:viewContractState

Questions to resolve
--------------------
What happens if we try to withdraw 0 ADA? What happens when the last ADA is withdrawn? Is it possible to delete the contract?
