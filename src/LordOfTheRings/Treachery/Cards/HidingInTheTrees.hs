module LordOfTheRings.Treachery.Cards.HidingInTheTrees (hidingInTheTrees) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HidingInTheTrees = HidingInTheTrees TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hidingInTheTrees :: TreacheryCard HidingInTheTrees
hidingInTheTrees = treachery HidingInTheTrees Cards.hidingInTheTrees

-- | When Revealed: Characters get -1 willpower and cannot attack until the end of the
-- round. The lasting effect is on the characters in play as it resolves.
-- (Its riddle is answered by the scenario.)
instance RunMessage HidingInTheTrees where
  runMessage msg t@(HidingInTheTrees attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifiers attrs c [WillpowerModifier (-1), CannotAttack]
      pure t
    _ -> HidingInTheTrees <$> runMessage msg attrs
