module LordOfTheRings.Quest.Cards.UpThePass (upThePass) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype UpThePass = UpThePass QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

upThePass :: QuestCard UpThePass
upThePass = quest UpThePass Cards.upThePass

-- | Setup: Add Caradhras to the staging area. Remove all copies of Snowstorm from the
-- encounter deck and set them aside, out of play. Put Arwen Undómiel into play under the
-- control of the first player.
-- When Revealed: Reveal 1 card from the encounter deck per player, and add it to the
-- staging area.
instance RunMessage UpThePass where
  runMessage msg q@(UpThePass attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Locations.caradhras)
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      traverse_ setAside $ filter (cardMatches $ cardIs Treacheries.snowstorm) deck
      searchEncounterDeckToStaging lead (cardIs Objectives.arwenUndomielTheRedhornGate)
      eachPlayer $ const revealEncounterCard
      pure $ UpThePass $ attrs {questFlipped = True}
    _ -> UpThePass <$> runMessage msg attrs
