module LordOfTheRings.Treachery.Cards.OrcHuntingParty (orcHuntingParty) where

import LordOfTheRings.Trait (Trait (Orc))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OrcHuntingParty = OrcHuntingParty TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHuntingParty :: TreacheryCard OrcHuntingParty
orcHuntingParty = treachery OrcHuntingParty Cards.orcHuntingParty

-- | When Revealed: Deal each Orc enemy in play a shadow card. Each Orc enemy gets -15
-- engagement cost until the end of the round. If there are no Orc enemies in the staging
-- area, Orc Hunting Party gains Surge.
instance RunMessage OrcHuntingParty where
  runMessage msg t@(OrcHuntingParty attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      orcs <- select (EnemyWithTrait Orc)
      for_ orcs dealShadowCard
      for_ orcs \o -> roundModifier attrs o (EngagementCostModifier (-15))
      staged <- selectAny (EnemyWithTrait Orc <> EnemyInStagingArea)
      unless staged $ revealEncounterCardAfter (toTarget attrs)
      pure t
    _ -> OrcHuntingParty <$> runMessage msg attrs
