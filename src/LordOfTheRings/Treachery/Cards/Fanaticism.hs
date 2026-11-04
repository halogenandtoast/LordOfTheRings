module LordOfTheRings.Treachery.Cards.Fanaticism (fanaticism) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition, Easterling))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Fanaticism = Fanaticism TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fanaticism :: TreacheryCard Fanaticism
fanaticism = treachery Fanaticism Cards.fanaticism

-- | While attached to an enemy, counts as a Condition attachment with the text: "Attached
-- enemy gets +1 Threat, +1 Attack, +1 Defense, and is immune to player card effects."
instance HasModifiersFor Fanaticism where
  getModifiersFor (Fanaticism a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Condition]
      modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, ImmuneToPlayerCardEffects]
    _ -> pure ()

-- | When Revealed: Attach to the Easterling enemy with the lowest Defense without a copy of
-- Fanaticism. Otherwise, Fanaticism gains surge.
instance RunMessage Fanaticism where
  runMessage msg t@(Fanaticism attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attached <-
        attachToEnemyWith attrs attrs.treacheryId attrs.treacheryCardCode (EnemyWithTrait Easterling) Lowest getEnemyDefense (const $ pure ())
      unless attached revealEncounterCard
      pure t
    _ -> Fanaticism <$> runMessage msg attrs
