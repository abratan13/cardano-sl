{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Cardano.Wallet.Kernel.PrefilterTx
       ( prefilterTxs
       , ourUtxo
       , prefilterTxs'
       , ourUtxo'
       ) where

import           Universum
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Pos.Core (HasConfiguration, Address (..))
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types (Utxo)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials,
                                                  eskToWalletDecrCredentials,
                                                  selectOwnAddresses)

import           Cardano.Wallet.Kernel.Types (ResolvedTx(..), ResolvedTxPair)

{-------------------------------------------------------------------------------
Pre-filter Tx Inputs and Outputs to "Ours" i.e. those that belong to the given Wallet.
Use the Wallet ESK to match "our" addresses.
-------------------------------------------------------------------------------}

prefilterTxs
    :: HasConfiguration
    => EncryptedSecretKey    -- ^ Wallet's secret key
    -> [ResolvedTx]
    -> [ResolvedTx]          -- ^ Prefiltered [(inputs, outputs)]
prefilterTxs (eskToWalletDecrCredentials -> wdc)
    = map (prefilterTx wdc)

prefilterTx :: WalletDecrCredentials
             -> ResolvedTx
             -> ResolvedTx
prefilterTx wdc ResolvedTx{..} =
    ResolvedTx  (ourResolvedTxPairs wdc rtxInputs)
                (ourUtxo_ wdc rtxOutputs)

ourResolvedTxPairs :: WalletDecrCredentials -> [ResolvedTxPair] -> [ResolvedTxPair]
ourResolvedTxPairs wdc = ours wdc (txOutAddress . toaOut . snd)

ourUtxo :: EncryptedSecretKey -> Utxo -> Utxo
ourUtxo (eskToWalletDecrCredentials -> wdc) = ourUtxo_ wdc

ourUtxo_ :: WalletDecrCredentials -> Utxo -> Utxo
ourUtxo_ wdc utxo = Map.fromList $ ourResolvedTxPairs wdc $ Map.toList utxo

ours :: WalletDecrCredentials
        -> (a -> Address)
        -> [a]
        -> [a]
ours wdc selectAddr rtxs = map fst $ selectOwnAddresses wdc selectAddr rtxs

{-------------------------------------------------------------------------------
Pre-filter Tx Inputs and Outputs to "Ours" i.e. those that belong to the given Wallet.
Use the Wallet Derived Addresses to match "our" addresses.
-------------------------------------------------------------------------------}

prefilterTxs'
    :: HasConfiguration
    => [Address]        -- ^ Wallet Derived Addresses
    -> [ResolvedTx]     -- ^ Resolved Txs to be filtered
    -> [ResolvedTx]     -- ^ Prefiltered [(inputs, outputs)]
prefilterTxs' ourAddrs
    = map (prefilterTx' ourAddrs)

prefilterTx' :: [Address]
             -> ResolvedTx
             -> ResolvedTx
prefilterTx' ourAddrs ResolvedTx{..} =
    ResolvedTx (ourResolvedTxPairs' ourAddrs rtxInputs)
               (ourUtxo' ourAddrs rtxOutputs)

ourUtxo' :: [Address] -> Utxo -> Utxo
ourUtxo' ourAddrs utxo
    = Map.fromList $ ourResolvedTxPairs' ourAddrs $ Map.toList utxo

ourResolvedTxPairs' :: [Address]         -- ^ "our" addresses
                    -> [ResolvedTxPair]  -- ^ Resolved Txs to be filtered
                    -> [ResolvedTxPair]  -- ^ return "our" Resolved Txs
ourResolvedTxPairs' ourAddrs addrs = filter isOurAddr addrs
    where ourAddrsS = Set.fromList ourAddrs
          getAddress = txOutAddress . toaOut . snd
          isOurAddr rTx = Set.member (getAddress rTx) ourAddrsS
