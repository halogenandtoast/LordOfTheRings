module LordOfTheRings.Hero.Cards.Elrond (elrond) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))

newtype Elrond = Elrond HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

elrond :: HeroCard Elrond
elrond = hero Elrond Cards.elrond

-- | You may spend resources from Elrond's resource pool to pay for spirit, leadership,
-- and tactics allies.
instance HasModifiersFor Elrond where
  getModifiersFor (Elrond a) = modifySelf a [GainResourceIconFor (CardWithType AllyType) s | s <- [Spirit, Leadership, Tactics]]

-- | Response: After a character is healed by another card effect, heal 1 damage on it.
instance HasAbilities Elrond where
  getAbilities (Elrond a) = [mkAbility a 1 $ response $ HealedByOtherWindow After (toSource a)]

instance RunMessage Elrond where
  runMessage msg h@(Elrond attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [t | Window _ (Healed t _) <- windows] \t -> healDamage attrs t 1
      pure h
    _ -> Elrond <$> runMessage msg attrs
