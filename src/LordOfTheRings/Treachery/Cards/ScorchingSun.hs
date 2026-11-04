module LordOfTheRings.Treachery.Cards.ScorchingSun (scorchingSun) where

import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (increaseTemperature)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ScorchingSun = ScorchingSun TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
scorchingSun :: TreacheryCard ScorchingSun
scorchingSun = treachery ScorchingSun Cards.scorchingSun

{- | When Revealed: Increase the temperature by 4.
Shadow: Either raise the temperature by 2, or return attacking enemy to the staging area
after this attack. The defending player chooses; the return is set up as a step of its
own, so that it is queued after the attack only once chosen.
-}
instance RunMessage ScorchingSun where
  runMessage msg t@(ScorchingSun attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      increaseTemperature 4
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "scorchingSun.choose"
        causedBy attrs
        labeled "scorchingSun.temperature" $ increaseTemperature 2
        labeled "scorchingSun.return" $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> ScorchingSun <$> runMessage msg attrs
