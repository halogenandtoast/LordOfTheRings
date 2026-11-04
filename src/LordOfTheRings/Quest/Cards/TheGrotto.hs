module LordOfTheRings.Quest.Cards.TheGrotto (theGrotto) where

import LordOfTheRings.Card (genEncounterCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterBan)
import LordOfTheRings.Trait (Trait (Grotto, Underwater))

newtype TheGrotto = TheGrotto QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theGrotto :: QuestCard TheGrotto
theGrotto = quest TheGrotto Cards.theGrotto

-- | While an Underwater location is the active location, players cannot play attachments
-- or allies.
instance HasModifiersFor TheGrotto where
  getModifiersFor (TheGrotto a) = when a.flipped $ underwaterBan a

-- | Forced: After a double-sided location is explored, add the top location from the
-- Grotto deck to the staging area, Grotto side faceup.
instance HasAbilities TheGrotto where
  getAbilities (TheGrotto a) = [mkAbility a 1 $ forced $ ExploredMatchingWindow When doubleSided | a.flipped]
   where
    doubleSided = CardMatchAny [CardWithTrait Grotto, CardWithTrait Underwater <> NotCard (cardIs Locations.drownedCave)]

{- | Setup: Set Shrine to Morgoth aside, out of play. Prepare the Grotto deck. Add the top 2
locations from the Grotto deck to the staging area, Grotto side faceup (the top 3 instead
if there are 3 or more players in the game). The first player takes control of Captain
Sahír, and the last player takes control of Na'asiyah (objective-ally sides faceup). The
scenario set Shrine to Morgoth aside and made the Grotto deck as it gathered the encounter
deck.
You cannot advance unless there are at least 3 Underwater locations in the victory display.
-}
instance RunMessage TheGrotto where
  runMessage msg q@(TheGrotto attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      players <- length <$> getRemainingPlayers
      addFromGrottoDeck (if players >= 3 then 3 else 2)
      addToStaging =<< genEncounterCard Objectives.captainSahirObjective
      addToStaging =<< genEncounterCard Objectives.naasiyahObjective
      TheGrotto <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      addFromGrottoDeck 1
      pure q
    _ -> TheGrotto <$> runConditionalStage threeUnderwater msg attrs
   where
    threeUnderwater = getsGame ((>= 3) . length . filter (cardMatches $ CardWithTrait Underwater) . (.victoryDisplay) . toAttrs . (.scenario))
