module LordOfTheRings.Treachery.Cards.OldHatreds (oldHatreds) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OldHatreds = OldHatreds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldHatreds :: TreacheryCard OldHatreds
oldHatreds = treachery OldHatreds Cards.oldHatreds

-- | Counts as a Condition attachment with the text: "Forced: After a player draws any number
-- of cards, raise his threat by 1."
instance HasAbilities OldHatreds where
  getAbilities (OldHatreds a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing | AttachedTo _ <- [a.placement]]

-- | Surge.
-- When Revealed: Attach to the current quest.
instance RunMessage OldHatreds where
  runMessage msg t@(OldHatreds attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (CardsDrawn pid _) <- windows] \pid -> encounterRaisesThreat pid 1
      pure t
    _ -> OldHatreds <$> runMessage msg attrs
