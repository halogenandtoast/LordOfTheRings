module LordOfTheRings.Ally.Cards.WarriorOfDale (warriorOfDale) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Trait (Trait (Weapon))

newtype WarriorOfDale = WarriorOfDale AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

warriorOfDale :: AllyCard WarriorOfDale
warriorOfDale = ally WarriorOfDale Cards.warriorOfDale

-- | Reduce the cost to play Weapon attachments on Warrior of Dale by 1. Warrior of Dale gets
-- +1 attack and gains ranged while it has a Weapon attachment.
instance HasModifiersFor WarriorOfDale where
  getModifiersFor (WarriorOfDale a) = do
    armed <- notNull <$> attachedWithTrait Weapon (toTarget a)
    modifySelf a $ ReduceCostToAttach (CardWithTrait Weapon) 1 : [m | armed, m <- [AttackModifier 1, GainKeyword Ranged]]
