module LordOfTheRings.Quest.Cards.NearingTheGate (nearingTheGate) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype NearingTheGate = NearingTheGate QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nearingTheGate :: QuestCard NearingTheGate
nearingTheGate = quest NearingTheGate Cards.nearingTheGate

-- | Setup: Remove Dark Pit from the encounter deck and place it aside, out of play. Add
-- Durin's Bane to the staging area. Shuffle the encounter deck.
-- When Revealed: Reduce each player's threat to 0. Reveal X cards from the encounter deck,
-- where X is one less than the number of players in the game, and add them to the staging
-- area.
instance RunMessage NearingTheGate where
  runMessage msg q@(NearingTheGate attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let find' def = find ((== toCardCode def) . toCardCode) deck
      traverse_ setAside (find' Locations.darkPit)
      for_ (find' Enemies.durinsBane) \bane -> do
        removeFromEncounterDeck bane
        addToStaging bane
      shuffleEncounterDeck
      eachPlayer \p -> lowerThreat p . (.threat) =<< getPlayer p
      players <- length <$> getRemainingPlayers
      revealEncounterCards (players - 1)
      pure $ NearingTheGate $ attrs {questFlipped = True}
    _ -> NearingTheGate <$> runMessage msg attrs
