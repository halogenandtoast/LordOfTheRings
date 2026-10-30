module LordOfTheRings.Quest.Cards.APresenceInTheDark (aPresenceInTheDark) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype APresenceInTheDark = APresenceInTheDark QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aPresenceInTheDark :: QuestCard APresenceInTheDark
aPresenceInTheDark = quest APresenceInTheDark Cards.aPresenceInTheDark

-- | Setup: Prepare the quest deck. Add The Nameless Fear to the staging area. Remove all
-- copies of A Foe Beyond from the encounter deck. Then, shuffle 1 copy of A Foe Beyond per
-- player back into the encounter deck. (The scenario gathers the deck so.)
-- When Revealed: Reveal 1 encounter card per player, and add it to the staging area.
-- Then, add A Presence in the Dark to your victory display.
instance RunMessage APresenceInTheDark where
  runMessage msg q@(APresenceInTheDark attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Enemies.theNamelessFear)
      turnOver attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      eachPlayer $ const revealEncounterCard
      addToVictoryDisplay attrs
      advanceQuest
      APresenceInTheDark <$> runMessage msg attrs
    _ -> APresenceInTheDark <$> runMessage msg attrs
