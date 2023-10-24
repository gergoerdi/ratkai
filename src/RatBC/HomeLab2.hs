{-# LANGUAGE RecordWildCards #-}
module RatBC.HomeLab2 where

import RatBC.Text
import RatBC.Game

import Control.Monad.Identity

reflowMessages :: Game Identity -> Game Identity
reflowMessages game@Game{..} = game
    { msgs1 = fmap (fmap $ wrapWords 40) msgs1
    , msgs2 = fmap (fmap $ wrapWords 40) msgs2
    }
