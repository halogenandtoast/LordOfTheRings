module LordOfTheRings.Treachery.Cards.TheDarkLordsSummons (theDarkLordsSummons) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (cardsUnderTheCrossRoads, getTheBlackGate, haradEnemy, lowestBy, placeUnderTheBlackGate, revealFromUnderTheCrossRoads)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheDarkLordsSummons = TheDarkLordsSummons TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDarkLordsSummons :: TreacheryCard TheDarkLordsSummons
theDarkLordsSummons = treachery TheDarkLordsSummons Cards.theDarkLordsSummons

{- | Peril.
When Revealed: Either place the lowest attack Harad enemy engaged with you facedown under
The Black Gate, or reveal the top facedown card from under The Cross-roads and put it into
play engaged with you. Only an option that can be done in full is offered (FAQ 1.44); with
enemies tied for the lowest attack, you choose among them.
-}
instance RunMessage TheDarkLordsSummons where
  runMessage msg t@(TheDarkLordsSummons attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      gate <- getTheBlackGate
      lowest <- lowestBy getEnemyAttack =<< select (haradEnemy <> EnemyEngagedWith (PlayerWithId you))
      under <- cardsUnderTheCrossRoads
      chooseOrRunOneM you do
        questionLabeled "theDarkLordsSummons.choose"
        causedBy attrs
        when (isJust gate && notNull lowest) $ labeled "theDarkLordsSummons.blackGate" do
          chooseOrRunOneM you do
            questionLabeled "theDarkLordsSummons.enemy"
            causedBy attrs
            targets lowest placeUnderTheBlackGate
        unless (null under) $ labeled "theDarkLordsSummons.crossRoads" $ revealFromUnderTheCrossRoads (Just you)
      pure t
    _ -> TheDarkLordsSummons <$> runMessage msg attrs
