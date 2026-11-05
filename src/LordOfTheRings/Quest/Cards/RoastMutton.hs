module LordOfTheRings.Quest.Cards.RoastMutton (roastMutton) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollEnemy)
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Trait (Trait (Troll))

newtype RoastMutton = RoastMutton QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

roastMutton :: QuestCard RoastMutton
roastMutton = quest RoastMutton Cards.roastMutton

{- | The encounter deck is not reset as it empties during the quest phase: once the players
have discarded it by questing, the stage advances (players' reports of the scenario on the
FFG forum, "We Must Away Ere Break of Day - Problem with the ending").
-}
instance HasModifiersFor RoastMutton where
  getModifiersFor (RoastMutton a) = when a.flipped $ modified a GameTarget [EncounterDeckDoesNotReset]

{- | When Revealed: Add the set-aside Troll enemies and the Troll Cave to the staging area.
Shuffle the encounter discard pile back into the encounter deck.

Forced: If there are no Troll enemies left in play, or if there are no cards left in the
encounter deck, advance to the next stage.

Any time players would place progress tokens on this quest, discard an equal number of
cards from the encounter deck instead. (Progress is placed on the active location before
triggering this effect.)

The Forced effect is checked whenever the board may have changed, as each window opens,
once the When Revealed effect has brought the Trolls.
-}
instance RunMessage RoastMutton where
  runMessage msg q@(RoastMutton attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RoastMutton <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      -- the discard pile is shuffled in first, that the deck not be counted empty as the
      -- Trolls arrive: the two effects do not depend on one another
      shuffleEncounterDiscardIntoDeck AnyCard
      addSetAsideToStaging (CardMatchAny [CardWithTrait Troll, cardIs Locations.trollCave])
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped -> do
      -- Resolve each discard before the next: Night Wanes Slow may immediately end
      -- the phase and cancel the progress still to be converted (Nightmare setup card).
      for_ [1 .. n] \_ -> discardTopOfEncounterDeck attrs 1
      pure q
    CheckWindows _ | attrs.flipped -> do
      checkStage attrs.id
      RoastMutton <$> runMessage msg attrs
    CheckStage qid | qid == attrs.id -> do
      trolls <- selectAny trollEnemy
      scenario <- getsGame (toAttrs . (.scenario))
      -- the Trolls set aside have yet to come: the When Revealed effect is still to resolve
      let waiting = any (cardMatches $ CardWithTrait Troll) (scenarioSetAside scenario)
      when (not waiting && (not trolls || null scenario.encounterDeck)) $ completeQuest attrs.id
      pure q
    _ -> RoastMutton <$> runMessage msg attrs
