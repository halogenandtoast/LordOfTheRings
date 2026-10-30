module LordOfTheRings.Enemy.Cards.ProfaneDead (profaneDead) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.IslandMap (activePosition)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ProfaneDead = ProfaneDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

profaneDead :: EnemyCard ProfaneDead
profaneDead = enemy ProfaneDead Cards.profaneDead

-- | Forced: When you travel to the right, Profane Dead makes an immediate attack against
-- the engaged player: as the players travel, before the location they travel to becomes
-- active, to one in a column further right on the Island Map.
instance RunMessage ProfaneDead where
  runMessage msg (ProfaneDead attrs) = case msg of
    TravelTo lid | EngagedWith pid <- attrs.placement -> do
      here <- activePosition
      there <- (.position) <$> getLocation lid
      when (fromMaybe False $ (\h t -> t.column > h.column) <$> here <*> there) $ immediateAttack pid attrs.id
      ProfaneDead <$> runMessage msg attrs
    _ -> ProfaneDead <$> runMessage msg attrs
