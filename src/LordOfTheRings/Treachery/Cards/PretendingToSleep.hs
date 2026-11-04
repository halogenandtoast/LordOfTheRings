module LordOfTheRings.Treachery.Cards.PretendingToSleep (pretendingToSleep) where

import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (advanceToStageFour, enemyProgress, getSmaugTheGolden, placeProgressOnSmaugTheGolden)
import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PretendingToSleep = PretendingToSleep TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge. Its burgle effect is read by burgle attempts only.
pretendingToSleep :: TreacheryCard PretendingToSleep
pretendingToSleep = treachery PretendingToSleep Cards.pretendingToSleep

{- | When Revealed: Put 1 progress token on Smaug the Golden. Then, spend X Baggins
resources or advance to stage 4A. X is the number of progress tokens on Smaug the Golden.
X is read once the progress is placed; with Smaug the Golden out of play it is 0, and
spending none costs nothing. The players spend as a group, the first player asked.
-}
instance RunMessage PretendingToSleep where
  runMessage msg t@(PretendingToSleep attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeProgressOnSmaugTheGolden attrs 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      smaug <- getSmaugTheGolden
      x <- maybe (pure 0) enemyProgress smaug
      lead <- getLead
      when (x > 0) $ groupMaySpendOr lead attrs Baggins x (pure ()) advanceToStageFour
      pure t
    _ -> PretendingToSleep <$> runMessage msg attrs
