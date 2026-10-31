module LordOfTheRings.Quest.Cards.CorsairConfrontation (corsairConfrontation) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype CorsairConfrontation = CorsairConfrontation QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairConfrontation :: QuestCard CorsairConfrontation
corsairConfrontation = quest CorsairConfrontation Cards.corsairConfrontation

-- | Sailing. Forced: At the end of the quest phase, if there is no Corsair Warship in play,
-- this stage is defeated. Where the players advance to is the scenario's.
instance HasAbilities CorsairConfrontation where
  getAbilities (CorsairConfrontation a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.flipped]

-- | When Revealed: Search the encounter deck, discard pile, and victory display for 1 copy
-- of Corsair Warship and add it to the staging area. Shuffle the encounter deck.
-- ponytail: the victory display only when the deck and discard pile have none
instance RunMessage CorsairConfrontation where
  runMessage msg q@(CorsairConfrontation attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      CorsairConfrontation <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let warship = (== toCardCode Enemies.corsairWarship) . toCardCode
      lead <- getLead
      case [c | EncounterCard c <- scenario.victoryDisplay, warship c] of
        card : _ | not (any warship $ scenario.encounterDeck <> scenario.encounterDiscard) -> do
          removeFromVictoryDisplay (EncounterCard card)
          addToStaging card
        _ -> searchEncounterDeckAndDiscard lead attrs (cardIs Enemies.corsairWarship)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM (selectNone $ EnemyWithCardCode $ toCardCode Enemies.corsairWarship) $ completeQuest attrs.id
      pure q
    _ -> CorsairConfrontation <$> runMessage msg attrs
