module RatBC.Game.FromImage where

import RatBC.Utils
import RatBC.Syntax
import RatBC.Words
import RatBC.Text
import RatBC.Game

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Array (listArray, range)
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.List.Split
import Data.Either
import Data.List (sort, nub)

fromImage :: BL.ByteString -> Game Identity
fromImage bs = game
  where
    game = Game
        { msgs1 = Identity $ listArray (1, fromIntegral $ length strings1) strings1
        , msgs2 = Identity $ listArray (1, fromIntegral $ length strings2) strings2
        , dict = Identity $ loadWords bs
        , enterRoom = Identity $
            let bs' = BL.tail $ deref bs 0x0827
                bss = map BL.pack . take numRooms . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
            in listArray (1, fromIntegral numRooms) $ map getStmts bss
        , afterTurn = Identity $
            let bs' = deref bs 0x082b
                bss = map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
            in getStmts (head bss)
        , interactiveGlobal = Identity $
            let bs' = deref bs 0x082d
            in interactive bs'
        , interactiveLocal = Identity $
            let bs' = BL.tail $ deref bs 0x0829
                bss = take numRooms $ map BL.pack . splitOn [0x00, 0x00, 0x00] . BL.unpack $ bs'
            in listArray (1, fromIntegral numRooms) $ map interactive bss
        , resetState = Identity $
            let bs' = deref bs 0x0835
            in BL.take (fromIntegral $ maxItem - minItem) bs'
        , helpMap = Identity $
            let bs' = BL.tail $ deref bs 0x0837
            in listArray (1, fromIntegral numRooms) $ BL.unpack bs'
        , minItem = minItem
        , maxItem = maxItem
        , startRoom = BL.index bs 0x083c
        , charSet = BL.take 0x400 . BL.drop 0x3000 $ bs
        , sprites = let bounds = (0, maximum spriteAddrs) in listArray bounds
                [ if i `elem` spriteAddrs then BL.take 63 . BL.drop (spriteBase + fromIntegral i * 64) $ bs else mempty
                | i <- range bounds
                ]
        }

    numRooms = 97 -- TODO

    minItem = BL.index bs 0x083a
    maxItem = BL.index bs 0x083b

    (strings1, strings2) = loadMessages bs

    getStmts bs =
      let (_, stmts) = partitionEithers $ runGet `flip` bs $ untilEOF getStmt
      in stmts

    interactive bs =
        let bss = map BL.pack . takeWhile (not . null) . splitOn [0x00, 0x00] . BL.unpack $ bs
        in [ InputDispatch (BL.unpack input) (getStmts action)
           | bs <- bss
           , let (input, action) = BL.break (== 0x00) bs
           , not $ BL.null action
           , action <- pure $ BL.tail action
           ]

    spriteAddrs = nub . sort $ execWriter $ traverseStmts_ (\_ -> tell . foldMap spritesOf) game
    spriteBase = 0x2100 -- TODO: is this fixed?

spritesOf :: Stmt -> [Word8]
spritesOf (SpriteOn _i addr _col _x _y) = [addr]
spritesOf (When00 _v body) = foldMap spritesOf body
spritesOf (WhenFF _v body) = foldMap spritesOf body
spritesOf _ = []
