module LordOfTheRings.Treachery.Cards.FierceSouthrons (fierceSouthrons) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (canPlaceUnderTheBlackGate, getTheBlackGate, haradEnemy, lowestBy, placeUnderTheBlackGate)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FierceSouthrons = FierceSouthrons TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fierceSouthrons :: TreacheryCard FierceSouthrons
fierceSouthrons = treachery FierceSouthrons Cards.fierceSouthrons

{- | Peril.
When Revealed: Either place the lowest threat Harad enemy in the staging area facedown under
The Black Gate and reveal an additional encounter card, or each enemy in the staging area
makes an immediate attack against you. Only an option that can be done in full is offered
(FAQ 1.44); with enemies tied for the lowest threat, you choose among them.
-}
instance RunMessage FierceSouthrons where
  runMessage msg t@(FierceSouthrons attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      gate <- getTheBlackGate
      lowest <- filterM canPlaceUnderTheBlackGate =<< lowestBy getEnemyThreat =<< select (haradEnemy <> EnemyInStagingArea)
      staged <- select EnemyInStagingArea
      chooseOrRunOneM you do
        questionLabeled "fierceSouthrons.choose"
        causedBy attrs
        when (isJust gate && notNull lowest) $ labeled "fierceSouthrons.blackGate" do
          chooseOrRunOneM you do
            questionLabeled "fierceSouthrons.enemy"
            causedBy attrs
            targets lowest placeUnderTheBlackGate
          revealEncounterCardAs you
        unless (null staged) $ labeled "fierceSouthrons.attack" $ for_ staged (immediateAttack you)
      pure t
    _ -> FierceSouthrons <$> runMessage msg attrs
