module LordOfTheRings.Quest.Cards.AttackOnPelargir (attackOnPelargir) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (shipLocation)
import LordOfTheRings.Trait (Trait (Ship))

newtype AttackOnPelargir = AttackOnPelargir QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

attackOnPelargir :: QuestCard AttackOnPelargir
attackOnPelargir = quest AttackOnPelargir Cards.attackOnPelargir

-- | Each Ship location in the staging area is immune to player card effects. The players
-- cannot defeat this stage while Great Corsair Ship is in play. (When this stage is
-- defeated, the players win the game: it is the last stage.)
instance HasModifiersFor AttackOnPelargir where
  getModifiersFor (AttackOnPelargir a) = when a.flipped do
    ships <- select (LocationInStagingArea <> LocationWithTrait Ship)
    for_ ships \l -> modified a l [ImmuneToPlayerCardEffects]
    greatShip <- selectAny (LocationWithCardCode $ toCardCode Locations.greatCorsairShip)
    when greatShip $ modifySelf a [CannotBeDefeated]

-- | Setup: Add The Corsair Fleet (Corsair side faceup) and Great Corsair Ship to the
-- staging area. Each player searches the encounter deck for a Ship location and adds it to
-- the staging area. Shuffle the encounter deck. (The scenario set the two aside as it
-- gathered the encounter deck.)
instance RunMessage AttackOnPelargir where
  runMessage msg q@(AttackOnPelargir attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (CardWithCode $ toCardCode Objectives.theCorsairFleet)
      addSetAsideToStaging (CardWithCode $ toCardCode Locations.greatCorsairShip)
      eachPlayer \p -> searchEncounterDeckToStaging p shipLocation
      AttackOnPelargir <$> runMessage msg attrs
    _ -> AttackOnPelargir <$> runMessage msg attrs
