module LordOfTheRings.Quest.Cards.WashedAway (washedAway) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType, TreacheryType), genEncounterCard, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet (EncounterSet (FoundationsOfStone))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (stageFourRevealed, stageFours)
import LordOfTheRings.Trait (Trait (Armor, Item, Light, Weapon))

newtype WashedAway = WashedAway QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

washedAway :: QuestCard WashedAway
washedAway = quest WashedAway Cards.washedAway

-- | When Revealed: Discard all Item, Armor, Weapon, Light cards and all encounter deck cards
-- from play. Shuffle all enemy and treachery cards in the encounter discard pile together
-- with the Foundations of Stone encounter set. This deck becomes the new encounter deck.
-- Remove all other encounter deck cards from the game. Then, starting with the first
-- player, each player draws a random stage 4 quest card. Remove all other stage 4 quest
-- cards from the game.
--
-- An objective leaves play to the staging area when discarded, and none is an enemy or a
-- treachery, so they are removed from the game. The drawn stages are resolved as the
-- scenario's helpers say: the first is put into play, the others follow it in the quest
-- deck, ahead of stage 5.
instance RunMessage WashedAway where
  runMessage msg q@(WashedAway attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      WashedAway <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      entities <- getsGame (.entities)
      let washed c = any (`member` cdCardTraits (toCardDef c)) [Item, Armor, Weapon, Light]
      for_ (filter washed $ toList entities.attachments) discard
      for_ entities.enemies discard
      for_ entities.treacheries discard
      for_ entities.locations discard
      for_ entities.objectives removeFromGame
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let kept c = cdCardType (toCardDef c) `elem` [EnemyType, TreacheryType]
      traverse_ removeFromEncounterDeck scenario.encounterDeck
      traverse_ removeFromEncounterDiscard (filter (not . kept) scenario.encounterDiscard)
      shuffleEncounterDiscardIntoDeck AnyCard
      shuffleSetAsideIntoEncounterDeck
        [c | c <- scenarioSetAside scenario, cdEncounterSet (toCardDef c) == Just FoundationsOfStone]
      players <- getRemainingPlayers
      drawn <- zip players <$> shuffleM stageFours
      cards <- traverse (genEncounterCard . snd) drawn
      case cards of
        next : others -> resetToStage next (others <> scenario.questDeck)
        [] -> advanceQuest
      for_ drawn $ uncurry (stageFourRevealed attrs)
      pure q
    _ -> WashedAway <$> runMessage msg attrs
