module LordOfTheRings.Enemy.Cards.UrdugEnemy (urdugEnemy) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gamePhase)
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

-- | His attack in the quest phase is this one: its damage sends him and Tiny back once it
-- resolves.
instance RunMessage UrdugEnemy where
  runMessage msg e@(UrdugEnemy attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> immediateAttack pid attrs.id
      pure e
    CheckWindows ws | notNull [() | Window After (DamagedByEnemyAttack _ eid) <- ws, eid == attrs.id] -> do
      questing <- getsGame ((== QuestPhase) . gamePhase)
      when questing do
        tiny <- select (EnemyWithCardCode $ toCardCode Cards.tinyEnemy)
        afterAttack attrs.id $ map (`PlaceEnemy` StagingArea) (attrs.id : tiny)
      UrdugEnemy <$> runMessage msg attrs
    _ -> UrdugEnemy <$> runMessage msg attrs
