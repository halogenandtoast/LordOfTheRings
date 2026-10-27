module LordOfTheRings.Quest.Cards.OrcOutpost (orcOutpost) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype OrcOutpost = OrcOutpost QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOutpost :: QuestCard OrcOutpost
orcOutpost = quest OrcOutpost Cards.orcOutpost

-- | When Revealed: Search the encounter deck and discard pile for Goblin Gate and add it
-- to the staging area, if able. Then, if there is no active location, Goblin Gate becomes
-- the active location.
instance RunMessage OrcOutpost where
  runMessage msg q@(OrcOutpost attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      OrcOutpost <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      active <- selectAny IsActiveLocation
      let isGate = cardMatches (cardIs Locations.goblinGate)
          -- added to the staging area, then made active at once when nothing is
          place = if active then addToStaging else putIntoPlayAsActiveLocation
      case (find isGate scenario.encounterDeck, find isGate scenario.encounterDiscard) of
        (Just gate, _) -> removeFromEncounterDeck gate >> place gate
        (_, Just gate) -> removeFromEncounterDiscard gate >> place gate
        -- already in the staging area
        _ ->
          unless active
            $ traverse_ makeActive
            =<< select (LocationWithCardCode (toCardCode Locations.goblinGate) <> LocationInStagingArea)
      shuffleEncounterDeck
      pure q
    _ -> OrcOutpost <$> runMessage msg attrs
