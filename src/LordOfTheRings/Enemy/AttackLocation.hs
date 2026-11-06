{- | A location treated as an enemy for the duration of a declared attack. The location
stays in play; this attack actor gives the ordinary combat machinery an EnemyId, so
declaration windows, ranged participation and attack modifiers work normally.
-}
module LordOfTheRings.Enemy.AttackLocation (AttackLocation (..), attackLocationEnemy) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (cbCardBuilder, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers, modifiedValue, threatOf)
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Placement (Placement (Unplaced))

newtype AttackLocation = AttackLocation EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

attackLocationEnemy :: LocationId -> PlayerId -> EnemyId -> LocationAttrs -> Enemy
attackLocationEnemy lid pid eid l =
  Enemy
    $ AttackLocation
    $ attrs
      { enemyAttackLocation = Just lid
      , enemyOwner = Just pid
      , enemyPlacement = Unplaced
      }
 where
  AttackLocation attrs = cbCardBuilder (enemy AttackLocation $ toCardDef l) l.cardId eid

instance HasModifiersFor AttackLocation where
  getModifiersFor (AttackLocation a) = for_ a.enemyAttackLocation \lid -> do
    location <- getsGame (Map.lookup lid . (.entities.locations))
    for_ location \l -> do
      defense <- modifiedValue threatOf (LocationTarget lid) (toAttrs l).threat
      mods <- getModifiers (LocationTarget lid)
      modifySelf a
        $ [DefenseModifier defense]
        <> [AttackedAsIfEngagedWith pid | Just pid <- [a.enemyOwner]]
        <> [m | m <- mods, m `elem` [CannotBeAttacked, CannotTakeCombatDamage, CannotTakeDamage]]

instance RunMessage AttackLocation where
  runMessage msg (AttackLocation attrs) = AttackLocation <$> runMessage msg attrs
