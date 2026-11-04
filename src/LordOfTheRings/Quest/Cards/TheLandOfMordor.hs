module LordOfTheRings.Quest.Cards.TheLandOfMordor (theLandOfMordor) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType), cdCardType, cdName, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountDoom.Helpers (firstTreacheryImmune, firstTreacherySurges)

newtype TheLandOfMordor = TheLandOfMordor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theLandOfMordor :: QuestCard TheLandOfMordor
theLandOfMordor = quest TheLandOfMordor Cards.theLandOfMordor

-- | Dire. This stage gets +4 quest points for each player. The first treachery revealed
-- each quest phase is immune to player card effects and gains surge.
instance HasModifiersFor TheLandOfMordor where
  getModifiersFor (TheLandOfMordor a) = when a.flipped do
    players <- length <$> getRemainingPlayers
    modifySelf a [QuestPointModifier (4 * players)]
    firstTreacheryImmune a

instance HasAbilities TheLandOfMordor where
  getAbilities (TheLandOfMordor a) = [firstTreacherySurges a 1 | a.flipped]

{- | Setup: Add Mount Doom and The Tower of Barad-dûr to the staging area. Make Brake of
Thorns the active location. Each player searches the encounter deck for a different
location and adds it to the staging area. Shuffle the encounter deck.

Brake of Thorns becomes the active location as the game is set up, so its Forced effect
answers it (FAQ 1.19: card effects during setup follow the normal game rules).
-}
instance RunMessage TheLandOfMordor where
  runMessage msg q@(TheLandOfMordor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.mountDoom)
      addSetAsideToStaging (cardIs Objectives.theTowerOfBaradDurMountDoom)
      addSetAsideToStaging (cardIs Locations.brakeOfThorns)
      doStep 1 msg
      eachPlayer \pid -> doStep 2 (WithTarget (PlayerTarget pid) msg)
      shuffleEncounterDeck
      TheLandOfMordor <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      brakes <- select (LocationWithCardCode (toCardCode Locations.brakeOfThorns) <> LocationInStagingArea)
      for_ (take 1 brakes) makeActive
      pure q
    -- the player adds a location, its title different from those the players have added
    DoStep 2 (WithTarget (PlayerTarget pid) (PerformSetup (isTarget attrs -> True))) -> do
      added <- traverse getLocation =<< select LocationInStagingArea
      let taken = map (cdName . toCardDef) added
          different c = cdCardType (toCardDef c) == LocationType && cdName (toCardDef c) `notElem` taken
      options <- filter different <$> getsGame ((.encounterDeck) . toAttrs . (.scenario))
      chooseOrRunOneM pid do
        questionLabeled "theLandOfMordor.addLocation"
        causedBy attrs
        showing (map EncounterCard options)
        targets options \card -> do
          removeFromEncounterDeck card
          addToStaging card
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    _ -> TheLandOfMordor <$> runMessage msg attrs
