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

 - The first player locks a sum of Ada in the contract, which is
   donated as a prize. The prize is protected by a secret password.

 - Any player can now try to guess the password, by submitting a
   'guess' transaction that attempts to withdraw some or all of the
   prize, along with a guess at the password, and a new password to
   replace the old one. If the guess is correct, and the contract
   contains enough Ada, then the guesser receives the withdrawal and
   the remainder of the prize is now protected by the new password. If
   the guess is wrong, the transaction is not accepted, and nothing
   changes.

As an extra wrinkle, when the first player locks the prize, a new
token is also forged. Only the player currently holding the token is
allowed to make a guess--which gives us an opportunity to illustrate
forging and passing around tokens.


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

Values and tokens
-----------------

Wallets contain 'values', which are mixtures of different quantities
of one or more types of token. The most common token is, of course, the Ada;
we can import functions manipulating Ada, and the ``Value`` type
itself, as follows:

.. literalinclude:: GameModel.hs
   :start-after: START_ADAIMPORTS
   :end-before:  END_ADAIMPORTS

With these imports, we can construct values in the Ada currency:

.. code-block:: text

  > Ada.lovelaceValueOf 1
  Value {getValue = Map {unMap = [(,Map {unMap = [(,1)]})]}}

We will also need a game token. After importing the ``Scripts`` module

.. literalinclude:: GameModel.hs
   :start-after: START_SCRIPTSIMPORT
   :end-before:  END_SCRIPTSIMPORT

we can define it as follows, applying a monetary policy defined in the code under test:

.. literalinclude:: GameModel.hs
   :start-after:  START_GAMETOKEN
   :end-before:   END_GAMETOKEN

The value of the token is (with long hash values abbreviated):

.. code-block:: text

  > gameTokenVal
  Value {getValue = Map {unMap = [(f687...,Map {unMap = [(guess,1)]})]}}

We can even construct a ``Value`` containing an Ada and a game token:

.. code-block:: text

  > Ada.lovelaceValueOf 1 <> gameTokenVal
  Value {getValue = Map {unMap =
    [(,Map {unMap = [(,1)]}),
     (f687...,Map {unMap = [(guess,1)]})]}}

If you inspect the output closely, you will see that a ``Value``
contains maps nested within another ``Map``. The outer ``Map`` is
indexed by hashes of monetary policy scripts, so each inner ``Map``
contains a bag of tokens managed by the same policy. Token names can
be chosen freely, and each policy can manage any number of its own
token types. In this case the game token is called a "guess", and the
script managing game tokens has the hash f687... A little confusingly,
the Ada token name is displayed as an empty string, as is the hash of
the corresponding monetary policy.

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

 - a ``Lock`` action to be performed by the first player when starting
   the game, containing the player's wallet (from which the Ada will
   be taken), the secret password, and the prize amount.
 - a ``Guess`` action to be performed by the other players, containing
   the player's wallet (to receive the prize), the player's guess, a
   new password, and the amount to be claimed if the guess is right.
 - a ``GiveToken`` action, to give the game token to a player so they
   can make a guess.

A generated test is called a ``Script``, and is (essentially) a
sequence of Action_. We can run tests by using `propRunScript_`_:

.. literalinclude:: GameModel.hs
    :start-after: START_GAME_PROPERTY
    :end-before: END_GAME_PROPERTY

When we test this property, ``quickCheck`` will generated random
scripts to be tested. But what is the ``handleSpec``?
`propRunScript_`_ needs to know which contract instances it should
create for use in this test, and the ``handleSpec`` tells it. Every
contract instance runs in an emulated wallet, and is identified by a
HandleKey_, which is another datatype associated with the
ContractModel_ class. It needs to be defined as a GADT, because it
also defines the types of the associated contract schema and contract
errors:

.. literalinclude:: GameModel.hs
    :start-after: START_HANDLE_KEY
    :end-before: END_HANDLE_KEY

Once the type of HandleKey_ is defined, we can construct our
HandleSpec_:

.. literalinclude:: GameModel.hs
    :start-after: START_HANDLE_SPEC
    :end-before: END_HANDLE_SPEC

This specifies that we should create one contract instance per wallet,
of ``G.contract``, the contract under test, identified by
``HandleKeys`` of the form ``WalletKey w``. Notice that we could test
several different contracts together with one contract model, and we
could run several contracts in each emulated wallet, in which case the
``HandleKeys`` would have to distinguish them. In this case, there is
one contract instance running in each wallet, and so the wallet itself
is enough to uniquely identify a contract instance.

Now we can run tests, although of course they will not yet succeed:

.. code-block:: text

  > quickCheck prop_Game
  *** Failed! (after 1 test and 1 shrink):
  Exception:
    GSM.hs:65:10-32: No instance nor default method for class operation arbitraryAction

The contract modelling library cannot generate test cases, unless *we*
specify how to generate Action_, which we will do next.

.. _ContractModel: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#t:ContractModel

.. _propRunScript_: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:propRunScript_

.. _HandleSpec: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#t:HandleSpec

Generating actions
------------------

To generate actions, we need to be able to generate wallets, guesses,
and suitable values of Ada, since these appear as action parameters.

.. literalinclude:: GameModel.hs
    :start-after: START_GENERATORS
    :end-before: END_GENERATORS

We choose wallets from the three available, and we choose passwords
from a small set, so that random guesses will often be
correct. We choose Ada amounts to be non-negative integers, because
negative amounts would be error cases that we choose not to test.

*** Is this really a good idea? Will a player who accidentally tries
to claim a negative sum actually lose money? ***

Now we can define a generator for Action_, as a method of the
ContractModel_ class:

.. literalinclude:: GameModel.hs
    :start-after: START_ARBITRARY
    :end-before: END_ARBITRARY

With this method defined, we can start to generate test cases. Using
``sample`` we can see what scripts look like:

.. code-block:: text

  > sample (arbitrary :: Gen (Script GameModel))
  Script
    [Lock (Wallet {getWallet = 2}) "hunter2" 5,
     Guess (Wallet {getWallet = 3}) "*******" "hello" 6,
     Guess (Wallet {getWallet = 1}) "secret" "*******" 10,
     Guess (Wallet {getWallet = 3}) "*******" "*******" 6,
     GiveToken (Wallet {getWallet = 3}),
     Guess (Wallet {getWallet = 2}) "hunter2" "hunter2" 15]
  .
  .

We can even run 'tests' now, although they don't do much yet:

.. code-block:: text

  > quickCheck prop_Game
  +++ OK, passed 100 tests:

  Actions (2263 in total):
  33.94% Lock
  33.89% Guess
  32.17% GiveToken

The output tells us the distribution of generated Actions, aggregated
across all the tests. We can see that each action was generated around
one third of the time, which is to be expected since our generator
does not weight them at all. Keep an eye on this table as we extend
our generation; if any Action_ disappears altogether, or is generated
very rarely, then this indicates a problem in our tests.

Modelling expectations
----------------------

The ultimate purpose of our tests is to check that funds are
transferred correctly by each operation--for example, that after a
guess, the guesser receives the requested Ada only if the guess was
correct. An important part of a ContractModel_ defines how funds
are expected to move. However, it's clear that in order to define how
we expect funds to move after a ``Guess``, we need to know more than
just where all the Ada are. We need to know:

- what the current secret password is, so we can decide whether or
  not the guess is correct.

- whether or not the guesser currently holds the game token, and so is
  entitled to make a guess.

- how much Ada is currently locked in the contract, so we can
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
forge_), deposits it in the creator's wallet, and withdraws the Ada
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
        enoughAda    <- (val <=)    <$> viewContractState gameValue
        when (correctGuess && holdsToken && enoughAda) $ do
            currentSecret $= new
            gameValue     $~ subtract val
            deposit w $ Ada.lovelaceValueOf val

``GiveToken`` just transfers the game token from one wallet to another using transfer_.

.. code-block:: haskell

    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewContractState hasToken
        transfer w0 w gameTokenVal
        hasToken $= Just w

At the end of each test, the ContractModel_ framework checks that
every wallet contains the tokens that the model says it should.

We can exercise the nextState_ function already by generating and
'running' tests, even though we have not yet connected these tests to
the real contract. Doing so immediately reveals a problem:

.. code-block:: text

  > quickCheck prop_Game
  *** Failed! (after 3 tests and 3 shrinks):
  Exception:
    Maybe.fromJust: Nothing
    CallStack (from HasCallStack):
      error, called at libraries/base/Data/Maybe.hs:148:21 in base:Data.Maybe
      fromJust, called at GSM0.hs:122:15 in main:GSM0
  Script
   [GiveToken (Wallet {getWallet = 1})]

Looking at the last two lines, we see the generated test script, and
the problem is evident: we generated a test *that only gives the game
token* to wallet 1, but this makes no sense because the game token has
not yet been forged--so the ``fromJust`` in the nextState_ function
fails. We will see how to prevent this in the next section.

Restricting test cases with preconditions
-----------------------------------------

As we just saw, not every sequence of actions makes sense as a test
case; we need a way to *restrict* test cases to be 'sensible'. Note
this is *not* the same as restricting tests to 'the happy path': we
*want* to test unexpected sequences of actions, and indeed, this is
part of the strength of property-based testing. But there are some
actions--like trying to give the game token to a wallet before it has been
forged--that are not even interesting to test. These are the cases
that we rule out by defining preconditions for actions; the effect is
to prevent such test cases ever being generated.

To introduce preconditions, we add a definition of the precondition_
method to our ContractModel_ instance.

.. code-block:: haskell

   precondition :: ModelState state -> Action state -> Bool

The precondition_ is parameterised on the entire model state, which
includes the contents of wallets as well as our contract state, so we
will need to extract this state as well as the fields we need from
it. For now, we just restrict ``GiveToken`` actions to states in which
the token exists:

.. code-block:: haskell

    precondition s (GiveToken _) = tok /= Nothing
        where
            tok = s ^. contractState . hasToken
    precondition s _             = True

Now if we try to run tests, something more interesting happens:

.. code-block:: text

  > quickCheck prop_Game
  *** Failed! Assertion failed (after 2 tests):
  Script
   [Lock (Wallet {getWallet = 1}) "hello" 0]
  Expected funds of W1 to change by Value {getValue = Map {unMap = [(f687...,Map {unMap = [(guess,1)]}),(,Map {unMap = [(,0)]})]}}
  but they changed by
  Value {getValue = Map {unMap = [(,Map {unMap = [(,0)]})]}}
  Test failed.
  Emulator log:
  [INFO] Slot 1: TxnValidate 4feb...
  [INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
                   Contract instance started
  [INFO] Slot 1: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 2}:
                   Contract instance started
  [INFO] Slot 1: 00000000-0000-4000-8000-000000000002 {Contract instance for wallet 3}:
                   Contract instance started

The test has failed, of course. The generated (and simplified) test case only performs one action:

.. code-block:: text

  Script
   [Lock (Wallet {getWallet = 1}) "hello" 0]

Wallet 1 attempts to create a game contract guarding zero
Ada. Inspecting the error message, we can see that wallet 1 ended up
with the wrong contents:

.. code-block:: text

  Expected funds of W1 to change by Value {getValue = Map {unMap =
    [(f687...,Map {unMap = [(guess,1)]}),(,Map {unMap = [(,0)]})]}}
  but they changed by
  Value {getValue = Map {unMap = [(,Map {unMap = [(,0)]})]}}

Our model predicted that wallet 1 would end up containing the game
token, but in fact its contents were unchanged.

In this test, we have actually performed actions on the emulated
blockchain, as the emulator log shows us: one transaction has been
validated, and we have started three contract instances (one for each
wallet in the test). But we have *not* created a game token for wallet
1, because thus far we have not defined how actions in a test should
be performed--so the ``Lock`` action in the test case behaves as a
no-op, which of course does not deposit a game token in wallet 1. It
is time to link actions in a test to the emulator.
   
Performing actions
------------------

So far we are generating Actions, but we have not yet linked them to
the contract they are supposed to test--so 'running' the tests, as we
did above, did not invoke the contract at all. To do so, we must import the emulator

.. literalinclude:: GameModel.hs
   :start-after: START_IMPORTEMULATOR
   :end-before:  END_IMPORTEMULATOR

Then we define the perform_ method of the ContractModel_ class:

.. code-block:: haskell

  perform :: HandleFun state
             -> ModelState state
             -> Action state
             -> Plutus.Trace.Emulator.EmulatorTrace ()

The job of the perform_ method in this case is just to invoke the
contract end-points, using the API defined in the code under test, and
transfer the game token from one wallet to another as specified by
``GiveToken`` actions.

.. code-block:: haskell

    perform handle s cmd = case cmd of
        Lock w new val -> do
            callEndpoint @"lock" (handle $ WalletKey w)
                         LockArgs{ lockArgsSecret = new
                                 , lockArgsValue = Ada.lovelaceValueOf val}
        Guess w old new val -> do
            callEndpoint @"guess" (handle $ WalletKey w)
                GuessArgs{ guessArgsOldSecret = old
                         , guessArgsNewSecret = new
                         , guessArgsValueTakenOut = Ada.lovelaceValueOf val}
        GiveToken w' -> do
            let w = fromJust (s ^. contractState . hasToken)
            payToWallet w w' gameTokenVal
            return ()

Every call to an end-point must be associated with one of the contract
handles defined in our ``handleSpec``; the ``handle`` argument to
perform_ lets us find the contract handle associated with each
HandleKey_.

For the most part, it is good practice to keep the perform_ function
simple: a direct relationship between actions in a test case and calls
to contract endpoints makes interpreting test failures much easier.

Helping shrinking work better by choosing test case actions well
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the definition of perform_ above, the ``GiveToken`` action is a little
surprising: when we call the emulator, we have to specify not only the
wallet to give the token *to*, but also the wallet to take the token
*from*. So why did we choose to define a ``GiveToken w`` action to
include in test cases, rather than an action ``PassToken w w'``, which
would correspond more directly to the code in perform_?

The answer is that using ``GiveToken`` actions instead helps
QuickCheck to shrink failing tests more effectively. QuickCheck
shrinks test cases by attempting to remove actions from
them--essentially replacing an action by a no-op. But consider a
sequence such as

.. code-block:: text

   PassToken w1 w2
   PassToken w2 w3

which transfers the game token in two steps from wallet 1 to
wallet 3. Deleting either one of these steps means the game token will
end up in the wrong place, probably causing the next steps in the test
to behave very differently (and thus, preventing this shrinking
step). But given the sequence

.. code-block:: text

   GiveToken w2
   GiveToken w3

the first ``GiveToken`` can be deleted without affecting the behaviour
of the second at all. Thus, by making token-passing steps independent
of each other, we make it easier for QuickCheck to shrink a failing
test without drastic changes to its behaviour.

Shrinking Actions
-----------------

Before starting to run tests seriously, it is useful to make sure that
any failing tests will shrink well to small examples. By default, the
contract modelling library tries to shrink tests by removing actions,
but it cannot know how to shrink the actions themselves. We can
specify this shrinking by defining the shrinkAction_ operation in the
ContractModel_ class:

.. code-block:: haskell

  shrinkAction :: ModelState state -> Action state -> [Action state]

This function returns a list of 'simpler' actions that should be tried
as replacements for the given Action_, when QuickCheck is simplifying
a failed test. In this case we define a shrinking function for wallets:

.. code-block:: haskell

   shrinkWallet :: Wallet -> [Wallet]
   shrinkWallet w = [w' | w' <- wallets, w' < w]

and shrink actions by shrinking the wallet and Ada parameters.

.. code-block:: haskell

    shrinkAction _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkAction _s (GiveToken w) =
        [GiveToken w' | w' <- shrinkWallet w]
    shrinkAction _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]

We choose to shrink an action either by shrinking a wallet parameter,
or by shrinking the amount of Ada. We choose not to shrink
password/guess parameters, because they are not really
significant--one password is as good as another in a failed test.



  
Debugging the model
-------------------

At this point, the contract model is complete, and tests are
runnable. However, they do not pass, and so we need to adapt either
the tests or the contract to resolve the inconsistencies revealed. Testing ``prop_Game`` now results in:

 .. code-block:: text

    > quickCheck prop_Game
    *** Failed! Falsified (after 6 tests and 3 shrinks):
    Script
     [Lock (Wallet {getWallet = 1}) "hunter2" 0]
    Expected funds of W1 to change by Value {getValue = Map {unMap = [(f687...,Map {unMap = [(guess,1)]}),(,Map {unMap = [(,0)]})]}}
    but they changed by
    Value {getValue = Map {unMap = [(,Map {unMap = [(,0)]})]}}
    Test failed.
    Emulator log:
    ... 49 lines of emulator log messages ...

In this test, wallet 1 attempts to lock zero Ada, and our model predicts
that wallet 1 should receive a game token--but this did not
happen. To understand why, we need to study the emulator log. Here are the relevant parts:

 .. code-block:: text

    ...
    [INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
                     Receive endpoint call: Object (fromList [("tag",String "lock"),...
    [INFO] Slot 1: W1: Balancing an unbalanced transaction:
                         Tx:
                           Tx 2542...:
                             {inputs:
                             outputs:
                               - Value {getValue = Map {unMap = [(,Map {unMap = [(,0)]})]}} addressed to
                                 ScriptAddress: d1e1...
    ...
    [INFO] Slot 1: W1: TxSubmit: 2542...
    [INFO] Slot 2: TxnValidate 2542...
    [INFO] Slot 2: W1: Balancing an unbalanced transaction:
                         Tx:
                           Tx 1eba...:
                             {inputs:
                                - 2542...!0
                                  Redeemer: <>
                             outputs:
                               - Value {getValue = Map {unMap = [(,Map {unMap = [(,0)]})]}} addressed to
                                 ScriptAddress: d1e1...
                             forge: Value {getValue = Map {unMap = [(f687...,Map {unMap = [(guess,1)]})]}}
    ...
    [INFO] Slot 2: W1: TxSubmit: 2d66...

Here we see the endpoint call to ``lock`` being received during slot
1, resulting in a transaction with ID ``2542...``, which pays zero Ada
to the contract script. The transaction is balanced (which has no
effect in this case), submitted, and validated by the emulator at
slot 2. Then another transaction, ``1eba...``, is created, which
forges the game token. This transaction is in turn balanced (resulting
in a new hash, ``2d66...``), and submitted without error--but although
no errors are reported, *this transaction is not validated*.

Since the transaction is submitted in slot 2, we would expect it to be
validated in slot 3. In fact, the problem here is just that the test
stopped too early, before the blockchain had validated this second
transaction. The solution is to delay two slots after a ``Lock``, to
give the blockchain time to complete its validations. Why two slots?
Because the ``Lock`` contract endpoint submits two transactions to the
blockchain. Likewise, we delay one slot after each of the other
actions. (If the delays we insert are too short, we will discover this
later via failed tests).

We can cause the emulator to delay a number of slots like this:

  .. code-block:: haskell
                  
    delay :: Int -> EmulatorTrace ()
    delay n = void $ waitNSlots (fromIntegral n)

We add a call to ``delay`` in each branch of perform_:

  .. code-block:: haskell

    perform handle s cmd = case cmd of
        Lock w new val -> do
            callEndpoint @"lock" (handle $ WalletKey w)
                         LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
            delay 2
        Guess w old new val -> do
            callEndpoint @"guess" (handle $ WalletKey w)
                GuessArgs{ guessArgsOldSecret = old
                         , guessArgsNewSecret = new
                         , guessArgsValueTakenOut = Ada.lovelaceValueOf val}
            delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. contractState . hasToken)
            payToWallet w w' gameTokenVal
            delay 1

We need to add corresponding calls to wait_ in the definition of
nextState_ also, so that our contract model remains in step with the
emulator.



*** I would really like to suggest rerunning the failed test at this
point, to check that this problem has been fixed. But copying and
pasting the test case is not working... first of all, we have to
import several modules into ghci to bring the constructors into
scope. Secondly, there is a type error involving the Action type from
StateModel, and the Action type from ContractModel. It's caused by
the := constructor, which is expecting a StateModel Action, but being
given a ContractModel Action. We'd better fix this.
***

*** Ulf's version of the code delays two slots after a lock. But why? How can one motivate that via debugging? If we just wait one step, then we get a totally bizarre error by doing a Lock followed by a GiveToken... which loses the token. Waiting two slots after a Lock seems to prevent that. But why? ***

*** Trying to debug nextState, I tried to display states using monitoring (monitoring counterexample). That isn't generating any output! Why not? ***

With these changes, tests fail for a different reason:

  .. code-block:: text

    > quickCheck prop_Game
    *** Failed! Falsified (after 5 tests and 2 shrinks):
    Script
     [Var 1 := Lock (Wallet {getWallet = 1}) "hunter2" 0,
      Var 2 := Lock (Wallet {getWallet = 1}) "hello" 0]
    Expected funds of W1 to change by Value {getValue = Map {unMap = [(f6879a6330ef3c0c4e9b73663bab99ab3a397984ceccb5c6569f8aeb3a3d61da,Map {unMap = [(guess,2)]}),(,Map {unMap = [(,0)]})]}}
    but they changed by
    Value {getValue = Map {unMap = [(f6879a6330ef3c0c4e9b73663bab99ab3a397984ceccb5c6569f8aeb3a3d61da,Map {unMap = [(guess,1)]}),(,Map {unMap = [(,0)]})]}}
    Test failed.
    Emulator log:
    ... 73 lines of emulator log messages ...

Looking at the failing test case,

  .. code-block:: text

    Script
     [Var 1 := Lock (Wallet {getWallet = 1}) "hunter2" 0,
      Var 2 := Lock (Wallet {getWallet = 1}) "hello" 0]

we can see that it does something unexpected: wallet 1 tries to lock
*twice*. Our model predicts that in this case *two* game tokens should
be forged, and wallet 1 should possess both.  But the error message
tells us that wallet 1 received only one game token, not two.

The emulator log output can be rather overwhelming, but we can eliminate the 'INFO' messages by running the test script with appropriate options. If we define

  .. code-block:: haskell
     

    import           Control.Monad.Freer.Log
    
    propGame' :: LogLevel -> Script GameModel -> Property
    propGame' l s = propRunScriptWithOptions
                        (set minLogLevel l defaultCheckOptions)
                        handleSpec
                        (\ _ -> pure True)
                        s

then we can re-run the test and see more succinct output:

  .. code-block:: text

    > quickCheck $ propGame' Warning
    *** Failed! Falsified (after 7 tests and 4 shrinks):
    Script
     [Var 3 := Lock (Wallet {getWallet = 1}) "hello" 0,
      Var 4 := Lock (Wallet {getWallet = 1}) "*******" 0]
    Expected funds of W1 to change by Value {getValue = Map {unMap = [(f6879a6330ef3c0c4e9b73663bab99ab3a397984ceccb5c6569f8aeb3a3d61da,Map {unMap = [(guess,2)]}),(,Map {unMap = [(,0)]})]}}
    but they changed by
    Value {getValue = Map {unMap = [(,Map {unMap = [(,0)]}),(f6879a6330ef3c0c4e9b73663bab99ab3a397984ceccb5c6569f8aeb3a3d61da,Map {unMap = [(guess,1)]})]}}
    Test failed.
    Emulator log:
    [WARNING] Slot 4: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
                        Contract instance stopped with error: GameSMError (ChooserError "Found 2 outputs, expected 1")
    
Now we see the problem: an error in the game implementation that
stopped the second contract call, because two outputs had been
created.





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
.. _shrinkAction: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:shrinkAction
.. _nextState: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:nextState
.. _initialState: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:initialState
.. _precondition: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:precondition
.. _perform: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:perform
.. _Spec: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#g:3
.. _`($=)`: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:`($=)`
.. _`($~)`: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:`($~)`

.. _forge: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:forge
.. _deposit: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:deposit
.. _withdraw: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:withdraw
.. _transfer: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:transfer
.. _viewContractState: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:viewContractState
.. _wait: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#v:wait
.. _Action: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#t:Action
.. _HandleKey: ../haddock/plutus-contract/html/Language-Plutus-Contract-Test-ContractModel.html#t:HandleKey

Questions to resolve
--------------------
What happens if we try to withdraw 0 Ada? What happens when the last Ada is withdrawn? Is it possible to delete the contract?

    Script [Var 1 := Lock (Wallet {getWallet = 1}) "hunter2" 0]
