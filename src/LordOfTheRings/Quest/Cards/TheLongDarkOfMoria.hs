module LordOfTheRings.Quest.Cards.TheLongDarkOfMoria (theLongDarkOfMoria) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (getDoom, removeDoomDamage)

newtype TheLongDarkOfMoria = TheLongDarkOfMoria QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLongDarkOfMoria :: QuestCard TheLongDarkOfMoria
theLongDarkOfMoria = quest TheLongDarkOfMoria Cards.theLongDarkOfMoria

-- | Forced: After a player optionally engages an enemy, remove 1 damage from Doom, Doom,
-- Doom.
instance HasAbilities TheLongDarkOfMoria where
  getAbilities (TheLongDarkOfMoria a) = [mkAbility a 1 $ forced $ AnyOptionallyEngagedWindow After | a.flipped]

{- | Setup: Set The Balrog, The Great Bridge, and Chamber of Mazarbul aside out of play (the
scenario does, as it gathers the encounter deck, Doom, Doom, Doom with them). Add Doom,
Doom, Doom to the staging area and place 10 damage tokens on it. Each player adds 1
different location to the staging area: each searches in turn, from the first player, for
a location unlike those already there. Then the encounter deck is shuffled.
-}
instance RunMessage TheLongDarkOfMoria where
  runMessage msg q@(TheLongDarkOfMoria attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Objectives.doomDoomDoom)
      doStep 1 msg
      turnOver attrs
      pure q
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      getDoom >>= traverse_ \oid -> placeTokens attrs (ObjectiveTarget oid) Damage 10
      doStep 2 (PerformSetup $ toTarget attrs)
      pure q
    DoStep n (PerformSetup (isTarget attrs -> True)) | n >= 2 -> do
      players <- getRemainingPlayers
      case drop (n - 2) players of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [() | Window _ (OptionallyEngaged _ _) <- ws] \_ -> removeDoomDamage attrs
      pure q
    _ -> TheLongDarkOfMoria <$> runMessage msg attrs
