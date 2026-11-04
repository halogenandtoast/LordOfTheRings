module LordOfTheRings.Quest.Cards.RaceToRivendell (raceToRivendell) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)
import LordOfTheRings.Trait (Trait (Nazgul))

newtype RaceToRivendell = RaceToRivendell QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

raceToRivendell :: QuestCard RaceToRivendell
raceToRivendell = quest RaceToRivendell Cards.raceToRivendell

-- | While at least 1 Nazgûl enemy is in play, Race to Rivendell gets +15 quest points.
instance HasModifiersFor RaceToRivendell where
  getModifiersFor (RaceToRivendell a) = when a.flipped do
    nazgul <- selectAny (EnemyWithTrait Nazgul)
    when nazgul $ modifySelf a [QuestPointModifier 15]

-- | Forced: At the end of the round, reduce the Ring-bearer's life by 1.
instance HasAbilities RaceToRivendell where
  getAbilities (RaceToRivendell a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | When Revealed (2B): Add Ford of Bruinen and The Witch-king to the staging area.

If the players defeat this stage, they have won the game.
-}
instance RunMessage RaceToRivendell where
  runMessage msg q@(RaceToRivendell attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      addSetAsideToStaging (cardIs Locations.fordOfBruinen)
      addSetAsideToStaging (cardIs Enemies.theWitchKing)
      RaceToRivendell <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      reduceRingBearersLife 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      pure q
    _ -> RaceToRivendell <$> runMessage msg attrs
