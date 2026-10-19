module LordOfTheRings.Ally.Cards.LorienGuide (lorienGuide) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype LorienGuide = LorienGuide AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lorienGuide :: AllyCard LorienGuide
lorienGuide = ally LorienGuide Cards.lorienGuide

-- | Response: After Lórien Guide commits to a quest, place 1 progress token on the
-- active location.
instance HasAbilities LorienGuide where
  getAbilities (LorienGuide a) =
    [restricted a 1 (exists IsActiveLocation) $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage LorienGuide where
  runMessage msg s@(LorienGuide a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      active <- select IsActiveLocation
      for_ active \l -> placeProgress a l 1
      pure s
    _ -> LorienGuide <$> runMessage msg a
