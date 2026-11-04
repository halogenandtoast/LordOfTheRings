module LordOfTheRings.Enemy.Cards.UrdugEnemy (urdugEnemy) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

-- | Urdug, enemy side faceup: Urdug's Gambit flips his objective-ally side
-- ("LordOfTheRings.Ally.Cards.Urdug") to it.
newtype UrdugEnemy = UrdugEnemy EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

urdugEnemy :: EnemyCard UrdugEnemy
urdugEnemy = enemy UrdugEnemy Cards.urdugEnemy

-- | Indestructible (on its card def). Immune to player card effects.
instance HasModifiersFor UrdugEnemy where
  getModifiersFor (UrdugEnemy a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | While Urdug is engaged with a player, he gains, "Forced: At the beginning of the quest
-- phase, Urdug makes an immediate attack. If this attack deals damage, return Urdug and
-- Tiny (if he is in play) to the staging area."
instance HasAbilities UrdugEnemy where
  getAbilities (UrdugEnemy a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | EngagedWith _ <- [a.placement]]

-- | "This attack" is the immediate attack the ability makes, marked for as long as it
-- lasts: its damage sends him and Tiny back once it resolves. Another attack of his in
-- the quest phase, as a treachery's, does not.
instance RunMessage UrdugEnemy where
  runMessage msg e@(UrdugEnemy attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> do
        attackModifier attrs attrs.id AttackMadeByOwnAbility
        immediateAttack pid attrs.id
      pure e
    CheckWindows ws | notNull [() | Window After (DamagedByEnemyAttack _ eid) <- ws, eid == attrs.id] -> do
      ownAttack <- elem AttackMadeByOwnAbility <$> getModifiers attrs.id
      when ownAttack do
        tiny <- select (EnemyWithCardCode $ toCardCode Cards.tinyEnemy)
        afterAttack attrs.id $ map (`PlaceEnemy` StagingArea) (attrs.id : tiny)
      UrdugEnemy <$> runMessage msg attrs
    _ -> UrdugEnemy <$> runMessage msg attrs
