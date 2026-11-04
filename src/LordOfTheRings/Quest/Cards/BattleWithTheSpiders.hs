module LordOfTheRings.Quest.Cards.BattleWithTheSpiders (battleWithTheSpiders) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (readyUnconsciousCharacterOf, spendToReadyUnconscious, unconsciousRestrictions)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEncounterDiscard))

newtype BattleWithTheSpiders = BattleWithTheSpiders QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

battleWithTheSpiders :: QuestCard BattleWithTheSpiders
battleWithTheSpiders = quest BattleWithTheSpiders Cards.battleWithTheSpiders

theSpidersGlade :: CardDef
theSpidersGlade = Locations.theSpidersGlade

-- | Unconscious characters cannot quest, attack, defend, collect resources, trigger
-- abilities, be poisoned, or ready (except by effects that target unconscious characters).
-- Progress cannot be placed on this stage while The Spider's Glade is in play.
instance HasModifiersFor BattleWithTheSpiders where
  getModifiersFor (BattleWithTheSpiders a) = when a.flipped do
    unconsciousRestrictions a
    glade <- selectAny (LocationWithCardCode $ toCardCode theSpidersGlade)
    when glade $ modifySelf a [ProgressLimit 0]

-- | Action: Spend 2 Baggins resources to ready an unconscious character you control.
instance HasAbilities BattleWithTheSpiders where
  getAbilities (BattleWithTheSpiders a) = [spendToReadyUnconscious a 1 | a.flipped]

{- | When Revealed: Add The Spider's Glade to the staging area: from out of play, where stage
1A set it aside, or from the discard pile, should the players' area it was in have left
the game.

If the players defeat this stage, they have won the game and discovered the treasure card
Bilbo's Magic Ring (the scenario's resolution records it).
-}
instance RunMessage BattleWithTheSpiders where
  runMessage msg q@(BattleWithTheSpiders attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      BattleWithTheSpiders <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      asideGlade <- getSetAside (CardWithCode $ toCardCode theSpidersGlade)
      discarded <- getsGame (filter ((== toCardCode theSpidersGlade) . toCardCode) . scenarioEncounterDiscard . toAttrs . (.scenario))
      case (asideGlade, discarded) of
        (glade : _, _) -> addToStaging glade
        ([], glade : _) -> addToStagingFromDiscard glade
        _ -> pure ()
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      readyUnconsciousCharacterOf attrs pid
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> BattleWithTheSpiders <$> runMessage msg attrs
