{-# LANGUAGE RecordWildCards #-}
module RatBC.Map where

import RatBC.Game
import RatBC.Pretty
import RatBC.Syntax

import Control.Monad.Identity
import Data.Array (assocs, indices)
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree
import Prettyprinter
import Prettyprinter.Render.Text
import Data.String (fromString)
import Data.List (sortBy, nubBy)
import Data.Function (on)

type Action = [Val]
type Room = Val

roomDot :: Game Identity -> DotGraph Node
roomDot game@Game{..} = graphToDot graphViz $ roomGraph game
  where
    graphViz = nonClusteredParams
        { fmtNode = \(_, room) -> [textLabel . fromString . show $ room]
        , fmtEdge = \(_, _, actions) -> [labelWords words | Just words <- pure actions]
        }

    labelWords = textLabel . renderLazy . layoutCompact . pprWords (runIdentity dict)

roomGraph :: Game Identity -> Gr Val (Maybe Action)
roomGraph Game{..} = mkGraph ns es
  where
    keyOf = fromIntegral

    ns = [(keyOf room, room) | room <- indices . runIdentity $ enterRoom]
    es = mconcat
        [ [(keyOf room, keyOf room', Just action) | (room, action, room') <- actionPaths ]
        , [(keyOf room, keyOf room', Nothing) | (room, room') <- otherPaths ]
        ]

    actionPaths =
        [ (room, action, room')
        | (room, dispatches) <- assocs . runIdentity $ interactiveLocal
        , let paths = [ (action, room')
                      | InputDispatch action stmts <- dispatches
                      , room' <- foldMap roomsMovedTo stmts
                      ]
        , (action, room') <- nubBy ((==) `on` snd) . sortBy (compare `on` snd) $ paths
        ]

    otherPaths =
        [ (room, room')
        | (room, stmts) <- assocs . runIdentity $ enterRoom
        , room' <- foldMap roomsMovedTo stmts
        ]


roomsMovedTo :: Stmt -> [Room]
roomsMovedTo (When00 _ body) = foldMap roomsMovedTo body
roomsMovedTo (WhenFF _ body) = foldMap roomsMovedTo body
roomsMovedTo (MoveTo room) = [room]
roomsMovedTo _ = []
