module LordOfTheRings.Quest.Cards.TheRoadToTharbad (theRoadToTharbad) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDunlandTrap.Helpers (eachPlayerSearchesToEngage)
import LordOfTheRings.Trait (Trait (BoarClan))

newtype TheRoadToTharbad = TheRoadToTharbad QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRoadToTharbad :: QuestCard TheRoadToTharbad
theRoadToTharbad = quest TheRoadToTharbad Cards.theRoadToTharbad

-- | Time 2. Forced: After the last time counter is removed from this stage, each player
-- discards his hand and draws 2 cards. Place 2 time counters on this stage.
instance HasAbilities TheRoadToTharbad where
  getAbilities (TheRoadToTharbad a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Setup: Set Chief Turch and Munuv Dûv Ravine aside, out of play. Make Old South Road
-- the active location. Each player searches the encounter deck for a Boar Clan enemy and
-- puts it into play engaged with him. Shuffle the encounter deck. (The encounter discard
-- pile is still empty, so searching it too finds nothing more.)
instance RunMessage TheRoadToTharbad where
  runMessage msg q@(TheRoadToTharbad attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies def = filter ((== toCardCode def) . toCardCode) deck
      traverse_ setAside $ concatMap copies [Enemies.chiefTurchTheDunlandTrap, Locations.munuvDuvRavine]
      for_ (take 1 $ copies Locations.oldSouthRoad) \card -> do
        removeFromEncounterDeck card
        putIntoPlayAsActiveLocation card
      eachPlayerSearchesToEngage attrs BoarClan
      pure $ TheRoadToTharbad $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        discardHand p
        drawCards p 2
      placeTimeCounters attrs attrs 2
      pure q
    _ -> TheRoadToTharbad <$> runMessage msg attrs
