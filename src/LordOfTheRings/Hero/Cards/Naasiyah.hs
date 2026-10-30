module LordOfTheRings.Hero.Cards.Naasiyah (naasiyah) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Naasiyah = Naasiyah HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

naasiyah :: HeroCard Naasiyah
naasiyah = hero Naasiyah Cards.naasiyah

-- | Resources in Na'asiyah's resource pool cannot be used to pay for allies.
instance HasModifiersFor Naasiyah where
  getModifiersFor (Naasiyah a) = modifySelf a [CannotPayFor (CardWithType AllyType)]

-- | Action: While Na'asiyah is attacking or defending, pay 1 resource from her resource
-- pool to give her +2 attack or +2 defense for this attack.
instance HasAbilities Naasiyah where
  getAbilities (Naasiyah a) =
    [ restricted a 1 (exists $ HeroWithId a.id <> HeroMatchAny [AttackingHero, DefendingHero])
        $ ActionAbility (ResourceCost (toTarget a) 1)
    ]

instance RunMessage Naasiyah where
  runMessage msg h@(Naasiyah attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseOneM pid do
        questionLabeled "naasiyah.choose"
        causedBy attrs
        labeled "naasiyah.attack" $ attackModifier attrs attrs (AttackModifier 2)
        labeled "naasiyah.defense" $ attackModifier attrs attrs (DefenseModifier 2)
      pure h
    _ -> Naasiyah <$> runMessage msg attrs
