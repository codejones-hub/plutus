.. highlight:: haskell
.. _contract_testing_tutorial:

Model-based testing of contracts
================================

Introduction to testing and stuff.

A guessing game contract
------------------------

Explain the contract. Double back-ticks for fixed-width font
``Language.PlutusTx.Coordination.Contracts.GameStateMachine``. Or use ``:hsobj:``:
:hsobj:`Language.PlutusTx.Coordination.Contracts.GameStateMachine`.
Link to other sections `like this <modelling_contracts>`_.

.. _modelling_contracts:

Modelling contracts
-------------------

You can include Haskell code using ``literalinclude``:

.. literalinclude:: GameModel.hs
   :start-after: START_MODELSTATE
   :end-before: END_MODELSTATE

.. note::
    This is a note.
