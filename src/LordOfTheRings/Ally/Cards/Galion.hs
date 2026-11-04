module LordOfTheRings.Ally.Cards.Galion (galion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Galion = Galion AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

galion :: AllyCard Galion
galion = ally Galion Cards.galion

-- | Cannot attack or defend.
instance HasModifiersFor Galion where
  getModifiersFor (Galion a) = modifySelf a [CannotAttackOrDefend]

-- | Response: After Galion enters play, he gets +1 [willpower] until the end of the round.
instance HasAbilities Galion where
  getAbilities (Galion a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage Galion where
  runMessage msg s@(Galion a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      roundModifier a a (WillpowerModifier 1)
      pure s
    _ -> Galion <$> runMessage msg a
