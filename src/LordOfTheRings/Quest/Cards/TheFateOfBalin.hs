module LordOfTheRings.Quest.Cards.TheFateOfBalin (theFateOfBalin) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheFateOfBalin = TheFateOfBalin QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFateOfBalin :: QuestCard TheFateOfBalin
theFateOfBalin = quest TheFateOfBalin Cards.theFateOfBalin

-- | Forced: At the end of the staging step, reveal the top X cards of the encounter deck,
-- adding all enemies to the staging area. Discard the other revealed cards without
-- resolving them. X is the number of players.
instance HasAbilities TheFateOfBalin where
  getAbilities (TheFateOfBalin a) = [mkAbility a 1 $ forced StagingStepEndsWindow]

-- | When Revealed: Remove Book of Mazarbul from the game.
-- If the players defeat this stage, they win the game.
instance RunMessage TheFateOfBalin where
  runMessage msg q@(TheFateOfBalin attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheFateOfBalin <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      books <- getsGame (filter ((== toCardCode Objectives.bookOfMazarbul) . toCardCode) . toList . (.entities.objectives))
      for_ books removeFromGame
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      discardTopOfEncounterDeck attrs players
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard [c | c <- cards, cdCardType (toCardDef c) == EnemyType]
      pure q
    _ -> TheFateOfBalin <$> runMessage msg attrs
