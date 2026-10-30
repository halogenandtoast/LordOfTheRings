module LordOfTheRings.Enemy.Cards.SlaveShip (slaveShip) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Ship))

newtype SlaveShip = SlaveShip EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

slaveShip :: EnemyCard SlaveShip
slaveShip = enemy SlaveShip Cards.slaveShip

-- | Boarding 1. Cannot have attachments.
instance HasModifiersFor SlaveShip where
  getModifiersFor (SlaveShip a) = modifySelf a [CannotHaveAttachments]

-- | For each point of damage that is dealt to a Ship objective by Slave Ship's attack,
-- deal 1 point of damage to a non-Ship character controlled by the defending player, as
-- well: the damage as it is dealt, reduced as it may have been, the defending player the
-- ship's controller, who divides it among his non-Ship characters.
instance RunMessage SlaveShip where
  runMessage msg (SlaveShip attrs) = case msg of
    DoStep 1 (DealDamage (EnemyAttackSource eid) target@(AllyTarget aid) n) | eid == attrs.id, n > 0 -> do
      ship <- selectAny (AllyWithId aid <> shipObjective)
      controller <- characterController target
      for_ controller \pid -> when ship do
        others <-
          selectCharacters
            (HeroControlledBy (PlayerWithId pid) <> not_ (HeroWithTrait Ship))
            (AllyControlledBy (PlayerWithId pid) <> not_ (AllyWithTrait Ship))
        unless (null others) $ dealDamageDividedAmong attrs pid others n
      SlaveShip <$> runMessage msg attrs
    _ -> SlaveShip <$> runMessage msg attrs
