module LordOfTheRings.Enemy.Cards.TheLordOfTheDead (theLordOfTheDeadErech) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype TheLordOfTheDead = TheLordOfTheDead EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLordOfTheDeadErech :: EnemyCard TheLordOfTheDead
theLordOfTheDeadErech = enemy TheLordOfTheDead Cards.theLordOfTheDeadErech

instance HasModifiersFor TheLordOfTheDead where
  getModifiersFor (TheLordOfTheDead a) = do
    modifySelf a [CannotHaveAttachments]
    for_ (placementController a.placement) \pid -> do
      characters <- charactersControlledBy pid
      for_ characters \target -> modified a target [Blank]
