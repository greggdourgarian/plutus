module Main (main) where

import           Language.PlutusCore
import           Language.PlutusCore.Evaluation.Machine.Cek (unsafeRunCek)

import           Control.Monad                              (void)
import qualified Data.ByteString.Lazy                       as BSL
import           Weigh

main :: IO ()
main = do
    ~(f, g) <- evalFiles
    let processor :: BSL.ByteString -> Either (Error AlexPosn) (Program TyName Name ())
        processor contents = void <$> (runQuoteT $ parseScoped contents)
        f' = processor f
        g' = processor g

    mainWith $ sequence_
        [ func "valid" (fmap (unsafeRunCek mempty)) f'
        , func "invalid" (fmap (unsafeRunCek mempty)) g'
        ]

    where evalFile0 = BSL.readFile "test/Evaluation/Golden/verifySignature.plc"
          evalFile1 = BSL.readFile "test/Evaluation/Golden/verifySignatureError.plc"
          evalFiles = (,) <$> evalFile0 <*> evalFile1
