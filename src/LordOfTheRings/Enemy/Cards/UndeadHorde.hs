module LordOfTheRings.Enemy.Cards.UndeadHorde (undeadHorde) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UndeadHorde = UndeadHorde EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

undeadHorde :: EnemyCard UndeadHorde
undeadHorde = enemy UndeadHorde Cards.undeadHorde

-- | Cannot have attachments.
-- For each excess point of combat damage dealt by Undead Horde (damage that is dealt beyond
-- the remaining hit points of the character damaged by its attack) you must discard 1 card
-- from the top of your deck.
instance HasModifiersFor UndeadHorde where
  getModifiersFor (UndeadHorde a) = modifySelf a [CannotHaveAttachments, ExcessDamageDiscardsFromDeck]
