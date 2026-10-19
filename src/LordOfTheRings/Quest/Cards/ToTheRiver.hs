module LordOfTheRings.Quest.Cards.ToTheRiver (toTheRiver) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ToTheRiver = ToTheRiver QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

toTheRiver :: QuestCard ToTheRiver
toTheRiver = quest ToTheRiver Cards.toTheRiver

-- | Setup: Each player reveals 1 card from the top of the encounter deck, and adds it to
-- the staging area.
-- When Revealed: Search the encounter deck for 1 Hill Troll (if one is not already in
-- play), and place it in the staging area. Shuffle the encounter deck.
-- Players cannot defeat this stage while any Hill Troll cards are in play.
instance RunMessage ToTheRiver where
  runMessage msg q@(ToTheRiver attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) -> do
      send $ FlipCard attrs.cardId
      eachPlayer $ const revealEncounterCard
      askToResolveWhenRevealed attrs
      pure $ ToTheRiver $ attrs {questFlipped = True}
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      trolls <- selectAny hillTrolls
      unless trolls $ searchEncounterDeckToStaging lead (cardIs Enemies.hillTroll)
      pure q
    _ -> ToTheRiver <$> runBlockedStage hillTrolls msg attrs
   where
    hillTrolls = EnemyWithCardCode (toCardCode Enemies.hillTroll)
