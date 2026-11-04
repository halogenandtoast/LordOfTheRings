module LordOfTheRings.Hero.Cards.Oin (oin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype Oin = Oin HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

oin :: HeroCard Oin
oin = hero Oin Cards.oin

-- | While you control at least 5 Dwarf characters, Óin gets +1 attack and gains the
-- [tactics] resource icon, not a printed one (FAQ 1.9).
instance HasModifiersFor Oin where
  getModifiersFor (Oin a) = do
    dwarves <- dwarfCount a.heroOwner
    when (dwarves >= 5) $ modifySelf a [AttackModifier 1, GainResourceIcon Tactics]

instance RunMessage Oin where
  runMessage msg (Oin attrs) = Oin <$> runMessage msg attrs
