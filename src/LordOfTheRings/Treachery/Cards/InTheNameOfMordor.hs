module LordOfTheRings.Treachery.Cards.InTheNameOfMordor (inTheNameOfMordor) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype InTheNameOfMordor = InTheNameOfMordor TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

inTheNameOfMordor :: TreacheryCard InTheNameOfMordor
inTheNameOfMordor = treachery InTheNameOfMordor Cards.inTheNameOfMordor

instance HasModifiersFor InTheNameOfMordor where
  getModifiersFor (InTheNameOfMordor a) = case a.placement of
    AttachedTo _ -> modifySelf a [GainTrait Condition]
    _ -> pure ()

instance HasAbilities InTheNameOfMordor where
  getAbilities (InTheNameOfMordor a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ forced $ FailedHideTestWindow After AnyPlayer]
    _ -> []

instance RunMessage InTheNameOfMordor where
  runMessage msg t@(InTheNameOfMordor a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      current <- getCurrentQuest
      for_ current $ attachTreachery a.id
      pure t
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | Window _ (FailedHideTest pid) <- windows] \pid -> discardRandomFromHand pid 1
      pure t
    _ -> InTheNameOfMordor <$> runMessage msg a
