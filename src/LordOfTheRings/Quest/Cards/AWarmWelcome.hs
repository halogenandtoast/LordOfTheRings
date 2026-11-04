module LordOfTheRings.Quest.Cards.AWarmWelcome (aWarmWelcome) where

import LordOfTheRings.Card (Card (PlayerCard), genPlayerCard)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (getTheLonelyMountain, lonelyMountainTreasures)

newtype AWarmWelcome = AWarmWelcome QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

aWarmWelcome :: QuestCard AWarmWelcome
aWarmWelcome = quest AWarmWelcome Cards.aWarmWelcome

-- | Skip the Quest phase.
instance HasModifiersFor AWarmWelcome where
  getModifiersFor (AWarmWelcome a) = when a.flipped $ modified a GameTarget [SkipQuestPhase]

-- | Forced: At the end of round, advance to stage 2.
instance HasAbilities AWarmWelcome where
  getAbilities (AWarmWelcome a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Search the encounter deck for Smaug the Golden, Smaug the Magnificent, The
Lonely Mountain, and A Bare Patch. Set Smaug the Magnificent and A Bare Patch aside, out
of play. Add Smaug the Golden and The Lonely Mountain to the staging area. Shuffle the
five Erebor treasure cards and stack them facedown under The Lonely Mountain. (The
scenario sets the four cards aside as it gathers.) The treasures are no player's until
one is taken into a player's hand.
-}
instance RunMessage AWarmWelcome where
  runMessage msg q@(AWarmWelcome attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging $ CardMatchAny [cardIs Enemies.smaugTheGolden, cardIs Locations.theLonelyMountain]
      doStep 1 msg
      AWarmWelcome <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      treasures <- shuffleM =<< traverse (`genPlayerCard` Nothing) lonelyMountainTreasures
      mountain <- getTheLonelyMountain
      for_ mountain \l -> placeUnderneath l (map PlayerCard treasures)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      advanceQuest
      pure q
    _ -> AWarmWelcome <$> runMessage msg attrs
