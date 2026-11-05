module LordOfTheRings.Treachery.Cards.DreadAndDespair (dreadAndDespair) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DreadAndDespair = DreadAndDespair TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Doomed 1.
dreadAndDespair :: TreacheryCard DreadAndDespair
dreadAndDespair = treachery DreadAndDespair Cards.dreadAndDespair

{- | When Revealed: Discard an ally you control. Until the end of the phase, add 1 to the
total threat in the staging area for each ally in your discard pile, counted as it changes.
Shadow: Discard a non-objective attachment you control.
-}
instance RunMessage DreadAndDespair where
  runMessage msg t@(DreadAndDespair attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      allies <- select (AllyControlledBy $ PlayerWithId you)
      chooseOrRunOneM you do
        questionLabeled "dreadAndDespair.discardAlly"
        causedBy attrs
        targets allies (discard attrs)
      phaseModifier attrs GameTarget (StagingThreatPerAllyInDiscardOf you)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure t
    _ -> DreadAndDespair <$> runMessage msg attrs
