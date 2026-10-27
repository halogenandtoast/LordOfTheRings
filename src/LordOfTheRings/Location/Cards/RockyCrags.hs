module LordOfTheRings.Location.Cards.RockyCrags (rockyCrags) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RockyCrags = RockyCrags LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rockyCrags :: LocationCard RockyCrags
rockyCrags = location RockyCrags Cards.rockyCrags

-- | Travel: Each player must deal 2 damage to 1 character he controls to travel here.
-- Shadow: attacking enemy gets +1 [Attack] for each progress token on the active location.
instance RunMessage RockyCrags where
  runMessage msg l@(RockyCrags a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> dealDamageToOneCharacter a p 2
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      progress <- activeLocationProgress
      when (progress > 0) $ enemyGetsAttack a eid progress
      pure l
    _ -> RockyCrags <$> runMessage msg a
