
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
      test `shouldThrow` \e -> seq (e :: Region.AlreadyClosed) True

  describe "free" $ do
    it "should call cleanup action" $ do
      res <- liftIO $ do
        ref <- newIORef 0
        region $ \r -> do
          (_, k) <- Region.alloc r (return 12) (writeIORef ref)
          Region.free k
          readIORef ref
      res `shouldBe` (12 :: Int)

    it "should throw when called twice" $ do
      let test = do
            r <- Region.open
            (_, k) <- Region.alloc r (return ()) (const $ return ())
            Region.free k
            Region.free k
      test `shouldThrow` \e -> seq (e :: Region.AlreadyFreed) True

  describe "region" $ do
    it "should free resources on exit" $ do
      ref <- liftIO $ newIORef False
      liftIO $ region $ \r ->
        void $ Region.alloc_ r
          (return ())
          (const $ writeIORef ref True)
      res <- liftIO $ readIORef ref
      res `shouldBe` True

  describe "close" $ do
    it "should free resources" $ do
      let test = do
            r <- Region.open
            (_, k) <- Region.alloc r (return ()) (const $ return ())
            Region.close r
            Region.free k
      test `shouldThrow` \e -> seq (e :: Region.AlreadyFreed) True

    it "should throw when called twice" $ do
      let test = do
            r <- Region.open
            Region.close r
            Region.close r
      test `shouldThrow` \e -> seq (e :: Region.AlreadyClosed) True

  describe "moveTo" $ do
    it "should remove resource from the original region" $ do
      let test = do
            r <- Region.open
            (_, k) <- Region.alloc r (return ()) (const $ return ())
            r' <- Region.open
            void $ k `Region.moveTo` r'
            Region.free k
      test `shouldThrow` \e -> seq (e :: SomeException) True

    it "should move resource to other region" $ do
      r <- Region.open
      (_, k) <- Region.alloc r (return ()) (const $ return ())
      r' <- Region.open
      k' <- k `Region.moveTo` r'
      Region.free k'

  describe "defer" $ do
    it "should add the action to region to be called on close" $ do
      ref <- liftIO $ newIORef False
      liftIO $ region $ \r ->
        void $ Region.defer r (writeIORef ref True)
      res <- liftIO $ readIORef ref
      res `shouldBe` True
