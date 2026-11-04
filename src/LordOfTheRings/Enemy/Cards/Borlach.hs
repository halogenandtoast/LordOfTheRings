module LordOfTheRings.Enemy.Cards.Borlach (borlach) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Borlach = Borlach EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

borlach :: EnemyCard Borlach
borlach = enemy Borlach Cards.borlach

-- | Immune to player card effects.
-- Only the engaged player can declare attackers or defenders against Borlach.
-- Forced: After Borlach attacks and destroys a character, any excess damage must be
-- assigned among characters you control: each point of it to another character the
-- defending player controls, as Cave-troll's.
instance HasModifiersFor Borlach where
  getModifiersFor (Borlach a) = modifySelf a [ImmuneToPlayerCardEffects, OnlyEngagedPlayerFights, ExcessDamageToOtherCharacters]

instance RunMessage Borlach where
  runMessage msg (Borlach attrs) = Borlach <$> runMessage msg attrs
