module LordOfTheRings.Ally.Cards.Ioreth (ioreth) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype Ioreth = Ioreth AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

ioreth :: AllyCard Ioreth
ioreth = ally Ioreth Cards.ioreth

-- | Cannot attack or defend.
instance HasModifiersFor Ioreth where
  getModifiersFor (Ioreth a) = modifySelf a [CannotAttackOrDefend]

-- | Action: Spend 1 lore resource and exhaust Ioreth. Then, heal 3 points of damage on a
-- character. Any player may trigger this action.
instance HasAbilities Ioreth where
  getAbilities (Ioreth a) =
    [anyPlayer $ mkAbility a 1 $ ActionAbility (Costs [SpendResourcesCost Lore 1, exhaustCost a])]

instance RunMessage Ioreth where
  runMessage msg s@(Ioreth a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOrRunOneM pid do
        questionLabeled "chooseCharacter"
        causedBy a
        targets damaged \c -> healDamage a c 3
      pure s
    _ -> Ioreth <$> runMessage msg a
