module LordOfTheRings.Location.Cards.RiverNinglor (riverNinglor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RiverNinglor = RiverNinglor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

riverNinglor :: LocationCard RiverNinglor
riverNinglor = location RiverNinglor Cards.riverNinglor

-- | While River Ninglor is the active location, remove 1 progress token from it and from
-- the current quest at the end of each round.
instance HasAbilities RiverNinglor where
  getAbilities (RiverNinglor a) = [restricted a 1 (exists $ LocationWithId a.id <> IsActiveLocation) $ forced RoundEndsWindow]

-- | Shadow: Remove 1 progress token from the current quest. (2 progress tokens instead if
-- this attack is undefended.)
instance RunMessage RiverNinglor where
  runMessage msg l@(RiverNinglor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      when (a.progress > 0) $ removeTokens a a Progress 1
      removeProgressFromQuest a 1
      pure l
    ResolveShadowEffect cid _ _ defender | cid == a.cardId -> do
      removeProgressFromQuest a (ifUndefended defender 2 1)
      pure l
    _ -> RiverNinglor <$> runMessage msg a
