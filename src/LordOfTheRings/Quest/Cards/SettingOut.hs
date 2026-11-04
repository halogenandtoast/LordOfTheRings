module LordOfTheRings.Quest.Cards.SettingOut (settingOut) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SettingOut = SettingOut QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

settingOut :: QuestCard SettingOut
settingOut = quest SettingOut Cards.settingOut

{- | Setup: Set Ford of Harnen (River Harnen, as stage 4A names it) and 1 copy of Uruk
Warg-rider aside, out of play: the scenario's setup does. Each player takes control of a
Tamed Mumak. Remove each remaining Tamed Mumak from the game. Create the Orcs' area with a
second quest deck consisting of stage 1C, 2C, 3C, and 4C as well as Uruk Chieftain.

When Revealed (1B): Each player searches the encounter deck for 1 different location and
adds it to the staging area. Shuffle the encounter deck. Advance to stage 2A. Each player
searches in turn, from the first player, for a location unlike those already in the
staging area.
-}
instance RunMessage SettingOut where
  runMessage msg q@(SettingOut attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      push $ ScenarioSpecific "orcs.prepare" Null
      doStep 1 msg
      pure $ SettingOut $ attrs {questFlipped = True}
    DoStep n (Flip (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) (Flip $ toTarget attrs)
        [] -> do
          shuffleEncounterDeck
          completeQuest attrs.id
      pure q
    _ -> SettingOut <$> runMessage msg attrs
