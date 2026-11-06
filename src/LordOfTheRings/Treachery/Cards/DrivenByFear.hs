module LordOfTheRings.Treachery.Cards.DrivenByFear (drivenByFear) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype DrivenByFear = DrivenByFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

drivenByFear :: TreacheryCard DrivenByFear
drivenByFear = treachery DrivenByFear Cards.drivenByFear

instance HasModifiersFor DrivenByFear where
  getModifiersFor (DrivenByFear a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerReducedToZero, Blank]
    _ -> pure ()

instance RunMessage DrivenByFear where
  runMessage msg t@(DrivenByFear a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      chooseOneM lead do
        questionLabeled "erech.drivenByFear"
        causedBy a
        targets heroes (attachTreachery a.id)
      pure t
    _ -> DrivenByFear <$> runMessage msg a
