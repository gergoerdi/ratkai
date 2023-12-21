{-# LANGUAGE TemplateHaskell, ApplicativeDo, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}

import Monomer
import Data.Array (Array, Ix, listArray, (!), indices, (//))
import Data.Word
import Control.Lens hiding (indices)
import Options.Applicative
import System.FilePath
import qualified Data.ByteString as BS
import RatBC.Picture
import TextShow (showt)
import Data.String (fromString)

data TVCColor = TVCColor
    { _r, _g, _b, _i :: Bool
    }
    deriving (Eq, Show)

makeLenses 'TVCColor

data AppModel = AppModel
    { _dump :: BS.ByteString
    , _pictureNum :: Word8
    , _palette :: Array Word8 TVCColor
    }
    deriving (Eq, Show)

makeLenses 'AppModel

data AppEvent
    = NoEvent
    | ChangePicture (Word8 -> Word8)


arrayLens :: (Ix i) => i -> Lens' s (Array i a) -> Lens' s a
arrayLens i l = lens (\s -> (s ^. getting l) ! i) (\s x -> s & l %~ (\xs -> xs // [(i, x)]))

-- TODO: This should operate on just the palette
paletteUI
    :: WidgetEnv AppModel AppEvent
    -> AppModel
    -> WidgetNode AppModel AppEvent
paletteUI wenv model = vstack . map paletteRow . indices . (^. palette) $ model
  where
    paletteRow idx = hstack
        [ label "" -- (showt idx)
        , imageMem_ ("color-target-" <> showt idx)
            (BS.pack $ case paletteC64 idx of (r, g, b) -> [r, g, b, 0xff]) (Size 1 1) [fitFill]
        , imageMem_ ("color-" <> showt idx <> fromString (show c))
            (BS.pack $ fromTVCColor c) (Size 1 1) [fitFill]
        , checkbox (lens . r)
        , checkbox (lens . g)
        , checkbox (lens . b)
        , checkbox (lens . i)
        ]
      where
        lens :: Lens' AppModel TVCColor
        lens = arrayLens idx palette

        c = model ^. lens

buildUI
    :: WidgetEnv AppModel AppEvent
    -> AppModel
    -> WidgetNode AppModel AppEvent
buildUI wenv model = vstack
    [ label $ "Image " <> showt idx
    , hstack
        [ imageMem_ ("target-" <> showt idx) pictTarget (Size 80 40) [fitEither, imageNearest]
        , imageMem_ ("preview-" <> showt idx <> fromString (show $ model ^. palette)) pictPreview (Size 80 40) [fitEither, imageNearest]
        ]
    , spacer
    , vstack
        [ paletteUI wenv model
        , textAreaV_ (fromString . show $ model ^. palette) (const NoEvent)
            [
            ]
        ]
    , hstack
        [ button "<<" (ChangePicture (subtract 1))
        , spacer
        , button ">>" (ChangePicture (+ 1))
        ]
    ] `styleBasic`
    [ padding 10
    ]
  where
    bs = model ^. dump
    idx = fromIntegral $ model ^. pictureNum

    bitmapAddr = 0xa000 + idx * 450
    colorsAddr = bitmapAddr + 0x190
    bitmap = BS.drop bitmapAddr bs
    colors = BS.drop colorsAddr bs

    pictTarget = BS.pack . concatMap applyPalette $ hiresPixels colors bitmap
      where
        applyPalette c = case paletteC64 c of
            (r, g, b) -> [r, g, b, 0xff]

    pictPreview = BS.pack . concatMap applyPalette $ hiresPixels colors bitmap
      where
        applyPalette c = fromTVCColor $ (model ^. palette) ! c

fromTVCColor :: TVCColor -> [Word8]
fromTVCColor (TVCColor r g b i) = [channel r, channel g, channel b, 0xff]
  where
    channel True = if i then 0xff else 0x80
    channel False = 0x00

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model = \case
    NoEvent -> mempty
    ChangePicture f -> [Model $ model & pictureNum %~ min 53 . f]

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo
    bs <- BS.readFile inputPath
    bs <- pure $ BS.drop 2 bs

    let model = AppModel
          { _dump = bs
          , _pictureNum = 1
          , _palette = listArray (0, 15) $
              [ TVCColor r g b i
              | r <- [True, False]
              , g <- [True, False]
              , b <- [True, False]
              , i <- [True, False]
              ]
          -- replicate 16 $ TVCColor False False False False
          }

    startApp model handleEvent buildUI config
  where
    config =
        [ appWindowTitle "TVC Palette Editor"
        , appTheme lightTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appInitEvent NoEvent
        ]

data Options = Options
    { inputPath :: FilePath
    }

options :: Parser Options
options = do
    inputPath <- strOption $ mconcat
        [ long "input"
        , short 'i'
        , metavar "FILENAME"
        , help "Memory dump"
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "RatBC TVC palette editor"
    ]
