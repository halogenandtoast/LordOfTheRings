module LordOfTheRings.Enemy.Cards.OrcChieftain (orcChieftain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype OrcChieftain = OrcChieftain EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

orcChieftain :: EnemyCard OrcChieftain
orcChieftain = enemy OrcChieftain Cards.orcChieftain

-- | Cannot have attachments. Allies cannot defend against Orc-chieftain.
instance HasModifiersFor OrcChieftain where
  getModifiersFor (OrcChieftain a) = modifySelf a [CannotHaveAttachments, CannotBeDefendedByAllies]

-- | Forced: At the beginning of the encounter phase, Orc-chieftain engages the first player.
instance HasAbilities OrcChieftain where
  getAbilities (OrcChieftain a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage OrcChieftain where
  runMessage msg e@(OrcChieftain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      unless (attrs.placement == EngagedWith lead) $ engageEnemy lead attrs.id
      pure e
    _ -> OrcChieftain <$> runMessage msg attrs
