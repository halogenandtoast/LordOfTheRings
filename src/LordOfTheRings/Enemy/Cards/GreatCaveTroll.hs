module LordOfTheRings.Enemy.Cards.GreatCaveTroll (greatCaveTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GreatCaveTroll = GreatCaveTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatCaveTroll :: EnemyCard GreatCaveTroll
greatCaveTroll = enemy GreatCaveTroll Cards.greatCaveTroll

-- | Immune to ranged damage. No attachments can be played on Great Cave-troll.
instance HasModifiersFor GreatCaveTroll where
  getModifiersFor (GreatCaveTroll a) = modifySelf a [ImmuneToRangedDamage, CannotHaveAttachments]
