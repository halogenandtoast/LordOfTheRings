module LordOfTheRings.Treachery.Cards.LyingInWait (lyingInWait) where

import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, turnFaceupResolving)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LyingInWait = LyingInWait TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lyingInWait :: TreacheryCard LyingInWait
lyingInWait = treachery LyingInWait Cards.lyingInWait

-- | When Revealed: The first player turns each hidden card in his play area faceup.
-- Resolve the "When Revealed" effects of each encounter card turned faceup by this effect.
-- If a location is turned faceup, add it to the staging area.
instance RunMessage LyingInWait where
  runMessage msg t@(LyingInWait attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      traverse_ (turnFaceupResolving lead) =<< getHiddenCards lead
      pure t
    _ -> LyingInWait <$> runMessage msg attrs
