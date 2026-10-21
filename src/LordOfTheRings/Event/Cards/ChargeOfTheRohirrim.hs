module LordOfTheRings.Event.Cards.ChargeOfTheRohirrim (chargeOfTheRohirrim) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Mount, Rohan))

newtype ChargeOfTheRohirrim = ChargeOfTheRohirrim EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Until the end of the phase, each Rohan character with a Mount attachment gets
-- +3 attack.
chargeOfTheRohirrim :: EventCard ChargeOfTheRohirrim
chargeOfTheRohirrim = event ChargeOfTheRohirrim Cards.chargeOfTheRohirrim

instance RunMessage ChargeOfTheRohirrim where
  runMessage msg e@(ChargeOfTheRohirrim attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      riders <- filterM (fmap notNull . attachedWithTrait Mount) =<< selectCharacters (HeroWithTrait Rohan) (AllyWithTrait Rohan)
      for_ riders \c -> phaseModifier attrs c (AttackModifier 3)
      pure e
    _ -> ChargeOfTheRohirrim <$> runMessage msg attrs
