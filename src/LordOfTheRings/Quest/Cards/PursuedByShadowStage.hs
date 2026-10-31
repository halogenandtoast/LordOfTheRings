module LordOfTheRings.Quest.Cards.PursuedByShadowStage (pursuedByShadowStage) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype PursuedByShadowStage = PursuedByShadowStage QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Search for an Exit (Pursued by Shadow), from Flight from Moria's nightmare set.
pursuedByShadowStage :: QuestCard PursuedByShadowStage
pursuedByShadowStage = quest PursuedByShadowStage Cards.pursuedByShadowStage

-- | No more than 1 progress can be placed on this stage each round.
instance HasModifiersFor PursuedByShadowStage where
  getModifiersFor (PursuedByShadowStage a) = modified a a [ProgressPerRoundLimit 1]

-- | Forced: At the end of the round, shuffle the discard pile into the encounter deck and
-- discard the top 5 cards. Resolve the "when revealed" effects of each copy of A Foe
-- Beyond discarded by this effect.
instance HasAbilities PursuedByShadowStage where
  getAbilities (PursuedByShadowStage a) =
    searchForAnExit a <> bypass a <> [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

-- | If the players defeat this stage, they win the game.
instance RunMessage PursuedByShadowStage where
  runMessage msg q@(PursuedByShadowStage attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      discardTopOfEncounterDeck attrs 5
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ resolveWhenRevealedFromDiscard $ filter ((== toCardCode Treacheries.aFoeBeyond) . toCardCode) cards
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped && attrs.progress + n >= attrs.questPoints -> do
      winGame
      pure q
    _ -> PursuedByShadowStage <$> runSearchForAnExit msg attrs
