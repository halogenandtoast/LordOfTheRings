module LordOfTheRings.Quest.Cards.OrcAmbush (orcAmbush) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcAmbush = OrcAmbush QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
orcAmbush :: QuestCard OrcAmbush
orcAmbush = quest OrcAmbush Cards.orcAmbush

-- | Each Orc enemy gets +1 threat, +1 attack, and +1 defense.
instance HasModifiersFor OrcAmbush where
  getModifiersFor (OrcAmbush a) = do
    orcs <- select (EnemyWithTrait Orc)
    for_ orcs \e -> modified a e [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]

-- | When Revealed: Each player must search the encounter deck and discard pile for an Orc
-- enemy and put it into play engaged with him. Shuffle the encounter deck.
instance RunMessage OrcAmbush where
  runMessage msg q@(OrcAmbush attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscardToEngage p attrs (CardWithType EnemyType <> CardWithTrait Orc)
      pure q
    _ -> OrcAmbush <$> runMessage msg attrs
