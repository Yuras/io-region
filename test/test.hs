
module Main
(
  main
)
where

import Data.IORef
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.IO.Region (region)
import qualified Control.IO.Region as Region
import qualified Control.IO.Region.Internal as Internal

import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "alloc_" $ do
    it "should return allocated resource" $ do
      res <- liftIO $ do
        r <- Region.open
        Region.alloc_ r
          (return 12)
          (const $ return ())
      res `shouldBe` (12 :: Int)

    it "should pass allocated resource to cleanup action" $ do
      res <- liftIO $ do
        ref <- newIORef 0
        region $ \r ->
          void $ Region.alloc_ r
            (return 12)
            (writeIORef ref)
        readIORef ref
      res `shouldBe` (12 :: Int)

    it "should throw when region is already closed" $ do
      let test = do
            r <- Region.open
            Region.close r
            Region.alloc_ r (return ()) (const $ return ())
      test `shouldThrow` \e -> seq (e :: Internal.AlreadyClosed) True

  describe "free" $ do
    it "should call cleanup action" $ do
      res <- liftIO $ do
        ref <- newIORef 0
        region $ \r -> do
          (_, k) <- Region.alloc r (return 12) (writeIORef ref)
          Region.free k
          readIORef ref
      res `shouldBe` (12 :: Int)

    context "when called twice" $ do
      it "should fail" $ do
        let test = do
              r <- Region.open
              (_, k) <- Region.alloc r (return ()) (const $ return ())
              Region.free k
              Region.free k
        test `shouldThrow` \e -> seq (e :: Internal.AlreadyFreed) True

  describe "region" $ do
    it "should free resources on exit" $ do
      ref <- liftIO $ newIORef False
      liftIO $ region $ \r ->
        void $ Region.alloc_ r
          (return ())
          (const $ writeIORef ref True)
      res <- liftIO $ readIORef ref
      res `shouldBe` True

    context "when region body fails" $ do
      it "should free resources" $ do
        ref <- liftIO $ newIORef False
        liftIO $ Internal.ignoreExceptions $ region $ \r -> do
          void $ Region.alloc_ r
            (return ())
            (const $ writeIORef ref True)
          fail "no way"
        res <- liftIO $ readIORef ref
        res `shouldBe` True

    context "when region body throws asynchronous exception" $ do
      it "should rethrow one" $ do
        let action = Region.region $ \r -> do
              Region.alloc_ r
                (return ())
                (const $ fail "no")
              throwIO UserInterrupt
        action `shouldThrow` \UserInterrupt -> True

    context "when cleanup throws asynchronous exception" $ do
      it "should rethrow one" $ do
        let action = Region.region $ \r -> do
              Region.alloc_ r
                (return ())
                (const $ throwIO UserInterrupt)
              fail "no"
        action `shouldThrow` \UserInterrupt -> True

  describe "close" $ do
    it "should free resources" $ do
      let test = do
            r <- Region.open
            (_, k) <- Region.alloc r (return ()) (const $ return ())
            Region.close r
            Region.free k
      test `shouldThrow` \e -> seq (e :: Internal.AlreadyFreed) True

    it "should throw when called twice" $ do
      let test = do
            r <- Region.open
            Region.close r
            Region.close r
      test `shouldThrow` \e -> seq (e :: Internal.AlreadyClosed) True

    it "should allow cleanups to be interrupted" $ do
      ref <- liftIO $ newIORef Unmasked
      r <- Region.open
      void $ Region.alloc_ r
        (return ())
        (const $ getMaskingState >>= writeIORef ref)
      Region.close r
      res <- liftIO $ readIORef ref
      res `shouldBe` MaskedInterruptible

    context "when cleanup fails" $
      it "should free other resources" $ do
        ref <- liftIO $ newIORef 0
        r <- Region.open
        void $ Region.alloc_ r
          (return ())
          (const $ modifyIORef' ref succ)
        void $ Region.alloc_ r
          (return ())
          (const $ fail "no way")
        void $ Region.alloc_ r
          (return ())
          (const $ modifyIORef' ref succ)
        Internal.ignoreExceptions $ Region.close r
        res <- liftIO $ readIORef ref
        res `shouldBe` 2

    context "when cleanup throws async exception" $ do
      it "should free other resources" $ do
        ref <- liftIO $ newIORef 0
        r <- Region.open
        void $ Region.alloc_ r
          (return ())
          (const $ modifyIORef' ref succ)
        void $ Region.alloc_ r
          (return ())
          (const $ throwIO UserInterrupt)
        void $ Region.alloc_ r
          (return ())
          (const $ modifyIORef' ref succ)
        Region.close r
          `catch` \e -> seq (e :: SomeException) (return ())
        res <- liftIO $ readIORef ref
        res `shouldBe` 2

      it "should rethrow the async exception" $ do
        r <- Region.open
        void $ Region.alloc_ r
          (return ())
          (const $ fail "no way")
        void $ Region.alloc_ r
          (return ())
          (const $ throwIO UserInterrupt)
        void $ Region.alloc_ r
          (return ())
          (const $ fail "no way")
        Region.close r
          `shouldThrow` (== UserInterrupt)

      it "should not allow subsequent cleanups to be interrupted" $ do
        ref <- liftIO $ newIORef Unmasked
        r <- Region.open
        void $ Region.alloc_ r
          (return ())
          (const $ getMaskingState >>= writeIORef ref)
        void $ Region.alloc_ r
          (return ())
          (const $ throwIO UserInterrupt)
        Region.close r
          `catch` \e -> seq (e :: SomeException) (return ())
        res <- liftIO $ readIORef ref
        res `shouldBe` MaskedUninterruptible

  describe "moveTo" $ do
    it "should remove resource from the original region" $ do
      r <- Region.open
      (_, k) <- Region.alloc r (return ()) (const $ return ())
      r' <- Region.open
      void $ k `Region.moveTo` r'
      Region.free k
        `shouldThrow` \Internal.AlreadyFreed -> True

    it "should move resource to other region" $ do
      r <- Region.open
      (_, k) <- Region.alloc r (return ()) (const $ return ())
      r' <- Region.open
      k' <- k `Region.moveTo` r'
      Region.free k'

    context "when target region is closed" $ do
      it "should fail" $ do
        r <- Region.open
        (_, k) <- Region.alloc r (return ()) (const $ return ())
        r' <- Region.open
        Region.close r'
        Region.moveTo k r'
          `shouldThrow` \Internal.AlreadyClosed -> True

  describe "defer" $ do
    it "should add the action to region to be called on close" $ do
      ref <- liftIO $ newIORef False
      liftIO $ region $ \r ->
        void $ Region.defer r (writeIORef ref True)
      res <- liftIO $ readIORef ref
      res `shouldBe` True
