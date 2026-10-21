module LordOfTheRings.Quest.Cards.ANewTerrorAbroad (aNewTerrorAbroad) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ANewTerrorAbroad = ANewTerrorAbroad QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aNewTerrorAbroad :: QuestCard ANewTerrorAbroad
aNewTerrorAbroad = quest ANewTerrorAbroad Cards.aNewTerrorAbroad

-- | Forced: At the beginning of the quest phase, the first player looks at the top 2 cards
-- of the encounter deck. Reveal and add 1 of those cards to the staging area, and discard
-- the other.
instance HasAbilities ANewTerrorAbroad where
  getAbilities (ANewTerrorAbroad a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage ANewTerrorAbroad where
  runMessage msg q@(ANewTerrorAbroad attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      revealOneOfTopEncounterCards lead attrs 2
      pure q
    _ -> ANewTerrorAbroad <$> runMessage msg attrs
