module LordOfTheRings.Objective.Cards.SarumansStaffTheRoadToIsengard (sarumansStaffTheRoadToIsengard) where

import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype SarumansStaffTheRoadToIsengard = SarumansStaffTheRoadToIsengard ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attached to Saruman by stage 3A, set aside until then.
sarumansStaffTheRoadToIsengard :: ObjectiveCard SarumansStaffTheRoadToIsengard
sarumansStaffTheRoadToIsengard = objective SarumansStaffTheRoadToIsengard Cards.sarumansStaffTheRoadToIsengard

-- | While Saruman has any hit points remaining, the players cannot win the game: the win
-- waits until he has as much damage as hit points (he is indestructible).
instance HasModifiersFor SarumansStaffTheRoadToIsengard where
  getModifiersFor (SarumansStaffTheRoadToIsengard a) = for_ (saruman a) \eid -> do
    e <- getEnemy eid
    hitPoints <- getEnemyHitPoints eid
    when (e.damage < hitPoints) $ modified a GameTarget [PlayersCannotWin]

-- | Forced: After a player draws any number of cards, Saruman attacks that player. Deal and
-- resolve 2 shadow cards for this attack. Then, remove 1 damage from Saruman.
instance HasAbilities SarumansStaffTheRoadToIsengard where
  getAbilities (SarumansStaffTheRoadToIsengard a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing | isJust (saruman a)]

instance RunMessage SarumansStaffTheRoadToIsengard where
  runMessage msg o@(SarumansStaffTheRoadToIsengard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (saruman attrs) \eid ->
        for_ (nub [pid | Window _ (CardsDrawn pid _) <- ws]) \pid -> do
          immediateAttackWithShadowCards 2 pid eid
          healDamage attrs eid 1
      pure o
    _ -> SarumansStaffTheRoadToIsengard <$> runMessage msg attrs

-- | The enemy it is attached to: Saruman.
saruman :: ObjectiveAttrs -> Maybe EnemyId
saruman a = case a.placement of
  AttachedTo (EnemyTarget eid) -> Just eid
  _ -> Nothing
