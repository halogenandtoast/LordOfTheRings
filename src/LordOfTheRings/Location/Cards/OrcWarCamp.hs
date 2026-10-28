module LordOfTheRings.Location.Cards.OrcWarCamp (orcWarCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcWarCamp = OrcWarCamp LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcWarCamp :: LocationCard OrcWarCamp
orcWarCamp = location OrcWarCamp Cards.orcWarCamp

-- | If an Orc enemy is in play, progress tokens cannot be placed on Orc War Camp while it
-- is in the staging area.
-- Shadow: attacking enemy gets +1 attack for each shadow card dealt to it.
instance RunMessage OrcWarCamp where
  runMessage msg l@(OrcWarCamp a) = case msg of
    PlaceTokens _ (isTarget a -> True) Progress _ | a.placement == StagingArea -> do
      orcs <- selectAny (EnemyWithTrait Orc)
      if orcs then pure l else OrcWarCamp <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      shadows <- length . (.shadowCards) <$> getEnemy eid
      enemyGetsAttack a eid shadows
      pure l
    _ -> OrcWarCamp <$> runMessage msg a
