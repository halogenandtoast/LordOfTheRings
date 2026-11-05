module LordOfTheRings.Ally.Cards.Turayn (turayn) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Turayn = Turayn AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Ered Mithrin campaign, from Journey Up the Anduin. Sentinel, on his card
-- def.
turayn :: AllyCard Turayn
turayn = ally Turayn Cards.turayn

-- | Response: After Turayn exhausts, choose another character. The next time that
-- character would be dealt damage this round, cancel 1 of that damage.
instance HasAbilities Turayn where
  getAbilities (Turayn a) = [mkAbility a 1 $ response $ ExhaustedWindow After (toTarget a)]

instance RunMessage Turayn where
  runMessage msg s@(Turayn a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      others <- filter (/= toTarget a) <$> selectCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "turayn.character"
        causedBy a
        targets others \c -> roundModifier a c (CancelNextDamage 1)
      pure s
    _ -> Turayn <$> runMessage msg a
