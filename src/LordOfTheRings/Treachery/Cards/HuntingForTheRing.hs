module LordOfTheRings.Treachery.Cards.HuntingForTheRing (huntingForTheRing) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HuntingForTheRing = HuntingForTheRing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 2.
huntingForTheRing :: TreacheryCard HuntingForTheRing
huntingForTheRing = treachery HuntingForTheRing Cards.huntingForTheRing

{- | When Revealed: Starting with the first player, each player must either return a
Nazgûl enemy engaged with him to the staging area or make a Hide 2 test. (The printed
card and RingsDB read "the first player"; Hall of Beorn's transcription "the last".) Each
player's choice, and the test it may bring, resolves before the next player's.
-}
instance RunMessage HuntingForTheRing where
  runMessage msg t@(HuntingForTheRing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (atMay players (n - 1)) \p -> do
        nazgul <- select (EnemyWithTrait Nazgul <> EnemyEngagedWith (PlayerWithId p))
        chooseOrRunOneM p do
          questionLabeled "huntingForTheRing.choose"
          causedBy attrs
          targets nazgul \e -> placeEnemy e StagingArea
          labeled "huntingForTheRing.hide" $ hideTest p attrs 2
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure t
    _ -> HuntingForTheRing <$> runMessage msg attrs
