module LordOfTheRings.Quest.Cards.InsideInformation (insideInformation) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (getSmaugTheGolden, getSmaugTheMagnificent)

newtype InsideInformation = InsideInformation QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

insideInformation :: QuestCard InsideInformation
insideInformation = quest InsideInformation Cards.insideInformation

{- | When Revealed: Set Smaug the Golden aside, out of play. Add Smaug the Magnificent and
A Bare Patch to the staging area.

Players cannot defeat this stage while Smaug the Magnificent is in play.
Forced: If the total number of progress on this stage is greater than the total remaining
hit points on Smaug the Magnificent, remove Smaug the Magnificent from play: he is set
aside, out of play, which no other card brings back. It is checked once progress is placed
here or damage on him.
If the players defeat this stage, they have won the game and discovered any treasure
successfully burgled from The Lonely Mountain (the scenario's resolution records them).
-}
instance RunMessage InsideInformation where
  runMessage msg q@(InsideInformation attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      InsideInformation <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ setAsideEnemy =<< getSmaugTheGolden
      addSetAsideToStaging $ CardMatchAny [cardIs Enemies.smaugTheMagnificent, cardIs Objectives.aBarePatch]
      pure q
    PlaceTokens _ target token _ | attrs.flipped, token `elem` [Progress, Damage] -> do
      smaug <- getSmaugTheMagnificent
      when (target == toTarget attrs || Just target == fmap toTarget smaug) $ doStep 1 msg
      InsideInformation <$> runBlockedStage magnificent msg attrs
    DoStep 1 (PlaceTokens {}) -> do
      smaug <- getSmaugTheMagnificent
      for_ smaug \sm -> do
        hitPoints <- getEnemyHitPoints sm
        damage <- getDamage (EnemyTarget sm)
        when (attrs.progress > hitPoints - damage) do
          setAsideEnemy sm
          push $ CheckStage attrs.id
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> InsideInformation <$> runBlockedStage magnificent msg attrs
   where
    magnificent = EnemyWithCardCode $ toCardCode Enemies.smaugTheMagnificent
