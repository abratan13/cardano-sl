{-# LANGUAGE TupleSections #-}

-- | Wallet unit tests
--
-- TODO: Take advantage of https://github.com/input-output-hk/cardano-sl/pull/2296 ?
module Main (main) where

import qualified Data.Set as Set
import           Data.List((!!))
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, shown, (%), formatToString)
import           Serokell.Util (mapJson, listJson)
import           Test.Hspec.QuickCheck
import           Universum
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

import qualified Pos.Block.Error as Cardano
import qualified Pos.Txp.Toil as Cardano
import           Pos.Util.Chrono
import           Pos.Core
import qualified Pos.Crypto as C

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import           Cardano.Wallet.Kernel.Types (ResolvedBlock(..))

import           UTxO.BlockGen
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.DSL              as DSL
import           UTxO.Interpreter
import           UTxO.PreChain
import           UTxO.Translate

import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           Util.Validated
import           Wallet.Abstract
import qualified Wallet.Basic          as Base
import qualified Wallet.Incremental    as Incr
import qualified Wallet.Prefiltered    as Pref
import qualified Wallet.Rollback.Basic as Roll
import qualified Wallet.Rollback.Full  as Full

import           UTxO.Crypto

import           Prelude (Show (..))

{-------------------------------------------------------------------------------
  Main test driver
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTranslateNoErrors $ withConfig $
        return $ hspec tests

-- | Debugging: show the translation context
_showContext :: IO ()
_showContext = do
    putStrLn $ runTranslateNoErrors $ withConfig $
      sformat build <$> ask
    putStrLn $ runTranslateNoErrors $
      let bootstrapTransaction' :: TransCtxt -> Transaction GivenHash Addr
          bootstrapTransaction' = bootstrapTransaction
      in sformat build . bootstrapTransaction' <$> ask

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: HasConfiguration => Spec
tests = describe "Wallet unit tests" $ do
    testTranslation
    testPureWallet
    testPureWallets
    testActiveWallet

{-------------------------------------------------------------------------------
  Utils
-------------------------------------------------------------------------------}

dslToCardanoUtxo :: IntCtxt GivenHash -> Utxo GivenHash Addr -> ValidatedT Cardano.Utxo
dslToCardanoUtxo intCtxt dslUtxo = do
    res <- catchTranslateErrors $ runIntT intCtxt dslUtxo
    case res of
        Left e -> throwError $ EqViolationIntEx e
        Right (utxo', _) ->
            return utxo'

dslToCardanoBlock :: IntCtxt GivenHash -> DSL.Block GivenHash Addr -> ValidatedT ResolvedBlock
dslToCardanoBlock intCtxt b = do
    res <- catchTranslateErrors $ runIntT intCtxt chain
    case res of
        Left e -> throwError $ EqViolationIntEx e
        Right (resolvedBlocks, _) ->
            return $ fromJust . head . getOldestFirst $ resolvedBlocks

    where blocks = OldestFirst [b]
          chain = Chain blocks

dslToCardanoTxAux :: IntCtxt GivenHash -> DSL.Transaction GivenHash Addr -> ValidatedT TxAux
dslToCardanoTxAux intCtxt t = do
    res <- catchTranslateErrors $ runIntT intCtxt t
    case res of
        Left e -> throwError $ EqViolationIntEx e
        Right (rawResolvedTx, _) ->
            return $ fst rawResolvedTx

poorActorESKAddrs :: Poor -> (C.EncryptedSecretKey, [Pos.Core.Address])
poorActorESKAddrs poorActor
    =  (encKpEnc rootEKP, [derivedAddress])
    where
        rootEKP = poorKey $ poorActor
        -- Currently there is only one _derived_ EKP/address per Poor Actor
        -- We use this (first and only) Address and discard the associated
        -- derived EKP
        (_, derivedAddress)
            = fromJust . head $ poorAddrs poorActor

getFirstPoorActorESK :: (C.EncryptedSecretKey, [Pos.Core.Address])
getFirstPoorActorESK
    = runTranslateNoErrors $ asks getFirstPoorActorESK_
    where
        getFirstPoorActorESK_ :: TransCtxt -> (C.EncryptedSecretKey, [Pos.Core.Address])
        getFirstPoorActorESK_ TransCtxt{..}
            =  poorActorESKAddrs poorActor
            where
                poorActors = Map.elems . actorsPoor $ tcActors
                poorActor = fromJust . head $ poorActors -- pick first Actor

{-------------------------------------------------------------------------------
  UTxO->Cardano translation tests
-------------------------------------------------------------------------------}

testTranslation :: Spec
testTranslation = do
    describe "Translation sanity checks" $ do
      it "can construct and verify empty block" $
        intAndVerifyPure emptyBlock `shouldSatisfy` expectValid

      it "can construct and verify block with one transaction" $
        intAndVerifyPure oneTrans `shouldSatisfy` expectValid

      it "can construct and verify example 1 from the UTxO paper" $
        intAndVerifyPure example1 `shouldSatisfy` expectValid

      it "can reject overspending" $
        intAndVerifyPure overspend `shouldSatisfy` expectInvalid

      it "can reject double spending" $
        intAndVerifyPure doublespend `shouldSatisfy` expectInvalid

    describe "Translation QuickCheck tests" $ do
      prop "can translate randomly generated chains" $
        forAll
          (intAndVerifyGen genValidBlockchain)
          expectValid

{-------------------------------------------------------------------------------
  Pure wallet tests
-------------------------------------------------------------------------------}
walletEquivalent' :: HasConfiguration
                 => Kernel.ActiveWallet
                 -> (Transaction GivenHash Addr -> Wallet GivenHash Addr)
                 -> Inductive GivenHash Addr
                 -> ValidatedT ()
walletEquivalent' cardanoW mkDSLWallet ind
    = do
        lift $ putText "New GenInductive Test"
        interpret' cardanoW mkDSLWallet p ind
  where
    p :: Inductive GivenHash Addr
      -> IntCtxt GivenHash
      -> Wallet GivenHash Addr
      -> ValidatedT ()
    p ind' intCtxt dslW =
            sequence_ [ cmpUtxo ind' intCtxt dslW
                      , cmpPending ind' intCtxt dslW]

        {-, TODO ...
        , cmp "availableBalance" availableBalance
        , cmp "totalBalance"     totalBalance
        , cmp "available"        available
        , cmp "change"           change
        , cmp "total"            total -}

    cmpUtxo :: Inductive GivenHash Addr
            -> IntCtxt GivenHash
            -> Wallet GivenHash Addr
            -> ValidatedT ()
    cmpUtxo ind' intCtxt dslW = do
        let cardanoPassiveW = Kernel.getPassiveWallet cardanoW
            dslUtxo = utxo dslW

        dslUtxoAsCardano <- dslToCardanoUtxo intCtxt dslUtxo

        cardanoUtxo <- lift $ Kernel.getWalletUtxo cardanoPassiveW
        checkEq "UTXO" ind' intCtxt dslUtxo dslUtxoAsCardano cardanoUtxo

    cmpPending :: Inductive GivenHash Addr
                -> IntCtxt GivenHash
                -> Wallet GivenHash Addr
                -> ValidatedT ()
    cmpPending ind' intCtxt dslW = do
        let cardanoPassiveW = Kernel.getPassiveWallet cardanoW
            dslPending = pending dslW

        dslPendingAsCardano <- Set.fromList <$>
                                   mapM (dslToCardanoTxAux intCtxt) (Set.toList dslPending)

        cardanoPending <- lift $ Kernel.getWalletPending cardanoPassiveW
        checkEq "PENDING" ind' intCtxt dslPending dslPendingAsCardano cardanoPending

    checkEq :: forall a b. (Buildable a, Buildable b, Eq b)
            => Text
            -> Inductive GivenHash Addr
            -> IntCtxt GivenHash
            -> a -> b -> b
            -> ValidatedT ()
    checkEq lbl ind' intCtxt dslVal dslAsCardanoVal cardanoVal
        = if dslAsCardanoVal == cardanoVal
            then do
                lift $ putText $ "inductive step... ok"
                return ()
            else throwError $
                     eqViolation lbl ind' (EqViolationEvidence intCtxt dslVal dslAsCardanoVal cardanoVal)

type ValidatedT a = TranslateT EqViolation IO a

data EqViolation = EqViolation {
    eqViolationName :: Text

    -- | Evidence that the equality was violated
  , eqViolationEvidence :: EqViolationEvidence

    -- | The 'Inductive' value at the point of the error
  , eqViolationInductive :: Inductive GivenHash Addr
  }
  | EqViolationIntEx IntException -- Interpretation Exception

instance Exception EqViolation

data EqViolationEvidence = forall a b. (Buildable a, Buildable b) => EqViolationEvidence {
    _eveIntCtxt :: IntCtxt GivenHash
  , _eveDslVal :: a
  , _eveDslAsCardanoVal :: b
  , _eveCardanoVal :: b
}

eqViolation :: Text
            -> Inductive GivenHash Addr
            -> EqViolationEvidence
            -> EqViolation
eqViolation name ind ev = EqViolation {
      eqViolationName      = name
    , eqViolationEvidence  = ev
    , eqViolationInductive = ind
    }

-- | Interpreter for 'Inductive'
--
-- Given a Cardano Active Wallet and Spec Wallet, evaluate an 'Inductive' wallet
-- in both DSL and Cardano worlds, comparing the wallets at each step.
interpret' :: HasConfiguration
          => Kernel.ActiveWallet
          -- ^ "Cardano Wallet"
          -> (Transaction GivenHash Addr -> Wallet GivenHash Addr)
          -- ^ "DSL Wallet" Builder
          -> (Inductive GivenHash Addr -> IntCtxt GivenHash -> Wallet GivenHash Addr -> ValidatedT ())
          -- ^ Predicate to check. The predicate is passed the 'Inductive'
          -- and 'DSl/Cardano Interpretation Context' at the point of the error,
          -- for better error messages.
          -> Inductive GivenHash Addr
          -- ^ 'Inductive' value to interpret
          -> ValidatedT ()
interpret' cardanoW mkDSLWallet' pCheckEqual ind'
      = void $ go ind'
      where
        cardanoPassiveW = Kernel.getPassiveWallet cardanoW

        -- Evaluate and verify the 'Inductive'
        -- Also returns the ledger after validation
        go :: Inductive GivenHash Addr -> ValidatedT (IntCtxt GivenHash, Wallet GivenHash Addr)
        go ind@(WalletBoot t)   = do
          -- DSL Wallet Boot
          let dslW = mkDSLWallet' t
              dslUtxo = utxo dslW

          -- Initialise DSL-to-Cardano Interpretation Context
          let intCtxt = initIntCtxt t

          -- Cardano Wallet Boot
          cardanoUtxo <- dslToCardanoUtxo intCtxt dslUtxo
          lift $ Kernel.init cardanoPassiveW cardanoUtxo

          pCheckEqual ind intCtxt dslW
          return (intCtxt,dslW)

        go ind@(ApplyBlock w b) = do
          -- DSL applyBlock
          (intCtxt, dslW) <- go w
          let dslW' = applyBlock dslW b

          -- Add Block Txs to DSL-to-Cardano Interpretation Context
          let intCtxt' = pushAll (toList b) intCtxt

          -- Cardano applyBlock
          cardanoBlock <- dslToCardanoBlock intCtxt' b
          lift $ Kernel.applyBlock cardanoPassiveW cardanoBlock

          pCheckEqual ind intCtxt' dslW'
          return (intCtxt', dslW')

        go ind@(NewPending w t) = do
          -- DSL New Pending
          (intCtxt, dslW) <- go w
          dslW' <- verifyNew ind intCtxt t dslW

          -- Cardano New Pending
          cardanoTx <- dslToCardanoTxAux intCtxt t
          _ <- lift $ Kernel.newPending cardanoW cardanoTx

          pCheckEqual ind intCtxt dslW'
          return (intCtxt, dslW')

        -- Verify the input
        -- If this fails, we provide the /entire/ 'Inductive' value so that it's
        -- easier to track down what happened.
        verifyNew :: Inductive GivenHash Addr -- ^ Inductive value at this point
                    -> IntCtxt GivenHash
                    -> Transaction GivenHash Addr
                    -> Wallet GivenHash Addr
                    -> ValidatedT (Wallet GivenHash Addr)
        verifyNew ind intCtxt tx w =
          case newPending w tx of
            Just w' -> return w'
            Nothing -> throwError $
                           eqViolation "DSL NewPending" ind
                            (EqViolationEvidence intCtxt tx () ()) -- (tx, w)
                           -- TODO create DSLError... generalise "ValidatedT e a"

newtype InductiveWithCtxt
    = InductiveWithCtxt (InductiveWithOurs GivenHash Addr
                        , C.EncryptedSecretKey
                        , [Pos.Core.Address])

testPureWallets :: HasConfiguration => Spec
testPureWallets =
    it "Test pure wallets - compare DSL and Cardano Wallets after each Inductive step" $
      forAll genInductive $ \(InductiveWithCtxt (ind, esk, addrs)) ->
                                (bracketActiveWallet esk addrs $ \cardanoW ->
                                    checkEquivalent cardanoW ind)

  where
    genInductive :: Hash GivenHash Addr
                 => Gen InductiveWithCtxt
    genInductive = do
      (fpc,transCtxt) <- runTranslateT $ do
                                    transCtxt' <- ask
                                    fpc' <- fromPreChain genValidBlockchain
                                    return (fpc',transCtxt')

      -- Select a Poor Actor (from the translation Context)
      -- and extract Root ESK and derived Addresses
      let poorActors = Map.elems . actorsPoor . tcActors $ transCtxt
      poorIx <- choose (0, length poorActors -1)
      let poorActor = poorActors !! poorIx
          poorActorIx = (IxPoor poorIx)
          (esk, addrs) = poorActorESKAddrs poorActor

      ind <- genFromBlockchainWithOurs (ours' poorActorIx) fpc
      return $ InductiveWithCtxt (ind, esk, addrs)

    ours' :: ActorIx -> Addr -> Bool
    ours' poorIx@(IxPoor _) addr = addrActorIx addr == poorIx
    ours' _ _ = False

    checkEquivalent :: HasConfiguration
                    => Kernel.ActiveWallet
                    -> InductiveWithOurs GivenHash Addr
                    -> Expectation
    checkEquivalent cardanoW (InductiveWithOurs addrs ind)
        = do
            res <- runTranslateT $ do
                      transCtxt <- ask
                      walletEquivalent' cardanoW (mkDSLWallet transCtxt addrs) ind

            shouldBe res ()

    mkDSLWallet :: TransCtxt -> Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    mkDSLWallet transCtxt = walletBoot Base.walletEmpty . (oursFromSet transCtxt)

    oursFromSet :: TransCtxt -> Set Addr -> Ours Addr
    oursFromSet transCtxt addrs addr = do
        guard (Set.member addr addrs)
        return $ fst (resolveAddr addr transCtxt)

testPureWallet :: Spec
testPureWallet = do
    it "Test pure wallets" $
      forAll genInductive $ \ind -> conjoin [
          checkInvariants "base"      ind baseEmpty
        , checkInvariants "incr"      ind incrEmpty
        , checkInvariants "pref"      ind prefEmpty
        , checkInvariants "roll"      ind rollEmpty
        , checkInvariants "full"      ind fullEmpty
        , checkEquivalent "base/incr" ind baseEmpty incrEmpty
        , checkEquivalent "base/pref" ind baseEmpty prefEmpty
        , checkEquivalent "base/roll" ind baseEmpty rollEmpty
        , checkEquivalent "base/full" ind baseEmpty fullEmpty
        ]

    it "Sanity check rollback" $ do
      let FromPreChain{..} = runTranslate $ fromPreChain oneTrans

          ours :: Ours Addr
          ours = oursFromSet $ Set.singleton r1

          w0, w1 :: Wallet GivenHash Addr
          w0 = walletBoot Full.walletEmpty ours fpcBoot
          w1 = applyBlocks w0 (chainBlocks fpcChain)
          w2 = rollback w1

      shouldNotBe (utxo w0) (utxo w1)
      shouldBe    (utxo w0) (utxo w2)
  where
    transCtxt = runTranslateNoErrors ask

    genInductive :: Hash h Addr => Gen (InductiveWithOurs h Addr)
    genInductive = do
      fpc <- runTranslateT $ fromPreChain genValidBlockchain
      n <- choose
        ( 1
        , length . filter (not . isAvvmAddr) . toList
        . ledgerAddresses $ fpcLedger fpc
        )
      genFromBlockchainPickingAccounts n fpc

    checkInvariants :: (Hash h a, Eq a, Buildable a)
                    => Text
                    -> InductiveWithOurs h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkInvariants label (InductiveWithOurs addrs ind) w =
        shouldBeValidated $
          walletInvariants label (w addrs) ind

    checkEquivalent :: (Hash h a, Eq a, Buildable a)
                    => Text
                    -> InductiveWithOurs h a
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> (Set a -> Transaction h a -> Wallet h a)
                    -> Expectation
    checkEquivalent label (InductiveWithOurs addrs ind) w w' =
        shouldBeValidated $
          walletEquivalent label (w addrs) (w' addrs) ind

    oursFromSet :: Set Addr -> Ours Addr
    oursFromSet addrs addr = do
        guard (Set.member addr addrs)
        return $ fst (resolveAddr addr transCtxt)

    baseEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    incrEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    prefEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    rollEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr
    fullEmpty :: Set Addr -> Transaction GivenHash Addr -> Wallet GivenHash Addr

    baseEmpty = walletBoot Base.walletEmpty . oursFromSet
    incrEmpty = walletBoot Incr.walletEmpty . oursFromSet
    prefEmpty = walletBoot Pref.walletEmpty . oursFromSet
    rollEmpty = walletBoot Roll.walletEmpty . oursFromSet
    fullEmpty = walletBoot Full.walletEmpty . oursFromSet

{-------------------------------------------------------------------------------
  Bracket Passive/Active Wallets
-------------------------------------------------------------------------------}

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWallet :: C.EncryptedSecretKey
                     -> [Pos.Core.Address]
                     -> (Kernel.PassiveWallet -> IO a)
                     -> IO a
bracketPassiveWallet esk addrs = Kernel.bracketPassiveWallet logMessage esk addrs
  where
   -- TODO: Decide what to do with logging
    logMessage _sev _txt = do
      print _txt

      return ()

-- | Initialize active wallet in a manner suitable for generator-based testing
bracketActiveWallet :: C.EncryptedSecretKey -> [Pos.Core.Address] -> (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet esk addrs test =
    (bracketPassiveWallet esk addrs) $ \passive ->
      Kernel.bracketActiveWallet passive diffusion $ \active ->
        test active

{-------------------------------------------------------------------------------
  Active wallet tests
-------------------------------------------------------------------------------}

testActiveWallet :: Spec
testActiveWallet = around bracketWallet $
    describe "Active wallet sanity checks" $ do
      it "initially has no pending transactions" $ \w ->
        Kernel.hasPending w `shouldReturn` False

-- | Initialize active wallet in a manner suitable for unit testing
bracketWallet :: (Kernel.ActiveWallet -> IO a) -> IO a
bracketWallet test = do
    let (esk, addrs) = getFirstPoorActorESK
    (bracketPassiveWallet esk addrs) $ \passive ->
      Kernel.bracketActiveWallet passive diffusion $ \active ->
        test active

-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
  walletSendTx = \_tx -> return False
}

{-------------------------------------------------------------------------------
  Example hand-constructed chains
-------------------------------------------------------------------------------}

emptyBlock :: Hash h Addr => PreChain h Identity ()
emptyBlock = preChain $ \_boot -> return $ \_fees ->
    OldestFirst [OldestFirst []]

oneTrans :: Hash h Addr => PreChain h Identity ()
oneTrans = preChain $ \boot -> return $ \((fee : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee)
                           ]
               , trExtra = ["t1"]
               }
    in OldestFirst [OldestFirst [t1]]

-- Try to transfer from R0 to R1, but leaving R0's balance the same
overspend :: Hash h Addr => PreChain h Identity ()
overspend = preChain $ \boot -> return $ \((fee : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 initR0
                           ]
               , trExtra = ["t1"]
               }
    in OldestFirst [OldestFirst [t1]]

-- Try to transfer to R1 and R2 using the same output
-- TODO: in principle this example /ought/ to work without any kind of
-- outputs at all; but in practice this breaks stuff because now we have
-- two identical transactions which would therefore get identical IDs?
doublespend :: Hash h Addr => PreChain h Identity ()
doublespend = preChain $ \boot -> return $ \((fee1 : fee2 : _) : _) ->
    let t1 = Transaction {
                 trFresh = 0
               , trFee   = fee1
               , trHash  = 1
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee1)
                           ]
               , trExtra = ["t1"]
               }
        t2 = Transaction {
                 trFresh = 0
               , trFee   = fee2
               , trHash  = 2
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r2 1000
                           , Output r0 (initR0 - 1000 - fee2)
                           ]
               , trExtra = ["t2"]
               }
    in OldestFirst [OldestFirst [t1, t2]]

-- Translation of example 1 of the paper, adjusted to allow for fees
--
-- Transaction t1 in the example creates new coins, and transaction t2
-- tranfers this to an ordinary address. In other words, t1 and t2
-- corresponds to the bootstrap transactions.
--
-- Transaction t3 then transfers part of R0's balance to R1, returning the
-- rest to back to R0; and t4 transfers the remainder of R0's balance to
-- R2.
--
-- Transaction 5 in example 1 is a transaction /from/ the treasury /to/ an
-- ordinary address. This currently has no equivalent in Cardano, so we omit
-- it.
example1 :: Hash h Addr => PreChain h Identity ()
example1 = preChain $ \boot -> return $ \((fee3 : fee4 : _) : _) ->
    let t3 = Transaction {
                 trFresh = 0
               , trFee   = fee3
               , trHash  = 3
               , trIns   = Set.fromList [ Input (hash boot) 0 ] -- rich 0
               , trOuts  = [ Output r1 1000
                           , Output r0 (initR0 - 1000 - fee3)
                           ]
               , trExtra = ["t3"]
               }
        t4 = Transaction {
                 trFresh = 0
               , trFee   = fee4
               , trHash  = 4
               , trIns   = Set.fromList [ Input (hash t3) 1 ]
               , trOuts  = [ Output r2 (initR0 - 1000 - fee3 - fee4) ]
               , trExtra = ["t4"]
               }
    in OldestFirst [OldestFirst [t3, t4]]

{-------------------------------------------------------------------------------
  Some initial values

  TODO: These come from the genesis block. We shouldn't hardcode them
  in the tests but rather derive them from the bootstrap transaction.
-------------------------------------------------------------------------------}

initR0 :: Value
initR0 = 11137499999752500

r0, r1, r2 :: Addr
r0 = Addr (IxRich 0) 0
r1 = Addr (IxRich 1) 0
r2 = Addr (IxRich 2) 0

{-------------------------------------------------------------------------------
  Verify chain
-------------------------------------------------------------------------------}

intAndVerifyPure :: PreChain GivenHash Identity a
                 -> ValidationResult GivenHash Addr
intAndVerifyPure = runIdentity . intAndVerify

intAndVerifyGen :: PreChain GivenHash Gen a
                -> Gen (ValidationResult GivenHash Addr)
intAndVerifyGen = intAndVerify

-- | Interpret and verify a chain, given the bootstrap transactions
intAndVerify :: (Hash h Addr, Monad m)
             => PreChain h m a -> m (ValidationResult h Addr)
intAndVerify = intAndVerifyChain

-- | Interpret and verify a chain, given the bootstrap transactions. Also
-- returns the 'FromPreChain' value, which contains the blockchain, ledger,
-- boot transaction, etc.
intAndVerifyChain :: (Hash h Addr, Monad m)
                  => PreChain h m a
                  -> m (ValidationResult h Addr)
intAndVerifyChain pc = runTranslateT $ do
    FromPreChain{..} <- fromPreChain pc
    let dslIsValid = ledgerIsValid fpcLedger
        dslUtxo    = ledgerUtxo    fpcLedger

    let f (resolvedBs, ctx) = (map rbBlock resolvedBs, ctx)
    intResult <- catchTranslateErrors $ f <$> runIntBoot fpcBoot fpcChain
    case intResult of
      Left e ->
        case dslIsValid of
          Valid     () -> return $ Disagreement fpcLedger (UnexpectedError e)
          Invalid _ e' -> return $ ExpectedInvalid' e' e
      Right (chain', ctxt) -> do
        let chain'' = fromMaybe (error "intAndVerify: Nothing")
                    $ nonEmptyOldestFirst chain'
        isCardanoValid <- verifyBlocksPrefix chain''
        case (dslIsValid, isCardanoValid) of
          (Invalid _ e' , Invalid _ e) -> return $ ExpectedInvalid e' e
          (Invalid _ e' , Valid     _) -> return $ Disagreement fpcLedger (UnexpectedValid e')
          (Valid     () , Invalid _ e) -> return $ Disagreement fpcLedger (UnexpectedInvalid e)
          (Valid     () , Valid (_undo, finalUtxo)) -> do
            (finalUtxo', _) <- runIntT ctxt dslUtxo
            if finalUtxo == finalUtxo'
              then return $ ExpectedValid
              else return $ Disagreement fpcLedger UnexpectedUtxo {
                         utxoDsl     = dslUtxo
                       , utxoCardano = finalUtxo
                       , utxoInt     = finalUtxo'
                       }

{-------------------------------------------------------------------------------
  Chain verification test result
-------------------------------------------------------------------------------}

data ValidationResult h a =
    -- | We expected the chain to be valid; DSL and Cardano both agree
    ExpectedValid

    -- | We expected the chain to be invalid; DSL and Cardano both agree
  | ExpectedInvalid {
        validationErrorDsl     :: Text
      , validationErrorCardano :: Cardano.VerifyBlocksException
      }

    -- | Variation on 'ExpectedInvalid', where we cannot even /construct/
    -- the Cardano chain, much less validate it.
  | ExpectedInvalid' {
        validationErrorDsl :: Text
      , validationErrorInt :: IntException
      }

    -- | Disagreement between the DSL and Cardano
    --
    -- This indicates a bug. Of course, the bug could be in any number of
    -- places:
    --
    -- * Our translatiom from the DSL to Cardano is wrong
    -- * There is a bug in the DSL definitions
    -- * There is a bug in the Cardano implementation
    --
    -- We record the error message from Cardano, if Cardano thought the chain
    -- was invalid, as well as the ledger that causes the problem.
  | Disagreement {
        validationLedger       :: Ledger h a
      , validationDisagreement :: Disagreement h a
      }

-- | Disagreement between Cardano and the DSL
--
-- We consider something to be "unexpectedly foo" when Cardano says it's
-- " foo " but the DSL says it's " not foo "; the DSL is the spec, after all
-- (of course that doesn't mean that it cannot contain bugs :).
data Disagreement h a =
    -- | Cardano reported the chain as invalid, but the DSL reported it as
    -- valid. We record the error message from Cardano.
    UnexpectedInvalid Cardano.VerifyBlocksException

    -- | Cardano reported an error during chain translation, but the DSL
    -- reported it as valid.
  | UnexpectedError IntException

    -- | Cardano reported the chain as valid, but the DSL reported it as
    -- invalid.
  | UnexpectedValid Text

    -- | Both Cardano and the DSL reported the chain as valid, but they computed
    -- a different UTxO
  | UnexpectedUtxo {
        utxoDsl     :: Utxo h a
      , utxoCardano :: Cardano.Utxo
      , utxoInt     :: Cardano.Utxo
      }

expectValid :: ValidationResult h a -> Bool
expectValid ExpectedValid = True
expectValid _otherwise    = False

expectInvalid :: ValidationResult h a -> Bool
expectInvalid (ExpectedInvalid _ _) = True
expectInvalid _otherwise            = False

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (ValidationResult h a) where
  build ExpectedValid = "ExpectedValid"
  build ExpectedInvalid{..} = bprint
      ( "ExpectedInvalid"
      % ", errorDsl:     " % build
      % ", errorCardano: " % build
      % "}"
      )
      validationErrorDsl
      validationErrorCardano
  build ExpectedInvalid'{..} = bprint
      ( "ExpectedInvalid'"
      % ", errorDsl: " % build
      % ", errorInt: " % build
      % "}"
      )
      validationErrorDsl
      validationErrorInt
  build Disagreement{..} = bprint
      ( "Disagreement "
      % "{ ledger: "       % build
      % ", disagreement: " % build
      % "}"
      )
      validationLedger
      validationDisagreement

instance (Hash h a, Buildable a) => Buildable (Disagreement h a) where
  build (UnexpectedInvalid e) = bprint ("UnexpectedInvalid " % build) e
  build (UnexpectedError e)   = bprint ("UnexpectedError " % shown) e
  build (UnexpectedValid e)   = bprint ("UnexpectedValid " % shown) e
  build UnexpectedUtxo{..}    = bprint
      ( "UnexpectedUtxo"
      % "{ dsl:     " % build
      % ", cardano: " % mapJson
      % ", int:     " % mapJson
      % "}"
      )
      utxoDsl
      utxoCardano
      utxoInt

instance Buildable InductiveWithCtxt where
    build (InductiveWithCtxt (ind, _, _)) = bprint ("InductiveWithCtxt " % build) ind

instance Buildable Cardano.Utxo where
  build = Cardano.formatUtxo

instance Buildable Kernel.Pending where
  build = bprint listJson . Set.toList

instance Buildable EqViolation where
    build (EqViolation{..}) = bprint
        ( "EqViolation "
        % "{ lbl:      "  % build
        % ", evidence: "  % build
        % ", inductive: " % build
        % "}"
        )
        eqViolationName
        eqViolationEvidence
        eqViolationInductive

    build (EqViolationIntEx intEx) = bprint
        ( "EqViolationIntEx "
        % "{ IntException:      "  % build
        % "}"
        )
        intEx

instance Buildable EqViolationEvidence where
  build (EqViolationEvidence{..}) = bprint
    ( "EqViolationEvidence "
    % "{ dslVal:          " % build
    % ", dslAsCardanoVal: " % build
    % ", cardanoVal:      " % build
    % "}"
    )
    _eveDslVal
    _eveDslAsCardanoVal
    _eveCardanoVal

instance Show EqViolation where
    show = formatToString build

instance Show EqViolationEvidence where
    show = formatToString build
