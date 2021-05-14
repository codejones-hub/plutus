module StaticData
  ( walletLibraryLocalStorageKey
  , walletDetailsLocalStorageKey
  , pabWallets
  ) where

import LocalStorage (Key(..))

walletLibraryLocalStorageKey :: Key
walletLibraryLocalStorageKey = Key "walletLibrary"

walletDetailsLocalStorageKey :: Key
walletDetailsLocalStorageKey = Key "walletDetails"

----------
pabWallets :: Key
pabWallets = Key "pabWallets"
