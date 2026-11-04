module LordOfTheRings.Enemy.Cards.GollumTheLandOfShadow (gollumTheLandOfShadow) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

-- | Gollum, his enemy side faceup (The Land of Shadow): flipped, he is the objective-ally of
-- "LordOfTheRings.Ally.Cards.SmeagolTheLandOfShadow".
newtype GollumTheLandOfShadow = GollumTheLandOfShadow EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumTheLandOfShadow :: EnemyCard GollumTheLandOfShadow
gollumTheLandOfShadow = enemy GollumTheLandOfShadow Cards.gollumTheLandOfShadow

-- | Immune to non-Fellowship player card effects.
-- Gollum engages the first player: no other player can engage him.
-- Defeated, he stays in play for his Forced effect to flip him.
instance HasModifiersFor GollumTheLandOfShadow where
  getModifiersFor (GollumTheLandOfShadow a) = do
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    modifySelf a
      $ [ImmuneToNonFellowshipPlayerCardEffects, EngagesOnly lead, StaysInPlayWhenDefeated]
      <> map CannotEngagePlayer others

-- | Forced: After Gollum engages the first player during the refresh phase, he makes an
-- immediate attack. (He engages the first player as its token passes; with one player it
-- never does, so he never attacks so: The Land of Shadow rules insert.)
instance HasAbilities GollumTheLandOfShadow where
  getAbilities (GollumTheLandOfShadow a) = [restricted a 1 (DuringPhase RefreshPhase) $ forced $ EnemyEngagedWindow After a.id]

{- | While Gollum is in play, he engages the first player: he engages him as he enters play,
whether put into play or turned over from Sméagol into the staging area; he follows the
first player token as it passes; and he never waits in the staging area.
Forced: When Gollum is defeated, flip him to Sméagol and exhaust Sméagol. His damage goes
with his enemy side (The Land of Shadow rules insert).
-}
instance RunMessage GollumTheLandOfShadow where
  runMessage msg e@(GollumTheLandOfShadow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      lead <- getLead
      for_ (take 1 [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == attrs.id, pid == lead]) \pid ->
        immediateAttack pid attrs.id
      pure e
    CheckWindows ws | notNull [() | Window After (EntersPlay t) <- ws, t == toTarget attrs] -> do
      lead <- getLead
      when (attrs.placement /= EngagedWith lead) $ engageEnemy lead attrs.id
      GollumTheLandOfShadow <$> runMessage msg attrs
    CheckWindows ws | notNull [() | Window After (EnemyDefeated eid) <- ws, eid == attrs.id] -> do
      flipOver attrs
      exhaust (AllyId $ unEnemyId attrs.id)
      GollumTheLandOfShadow <$> runMessage msg attrs
    SetFirstPlayer pid | attrs.placement /= EngagedWith pid -> do
      engageEnemy pid attrs.id
      GollumTheLandOfShadow <$> runMessage msg attrs
    PlaceEnemy eid StagingArea | eid == attrs.id -> do
      lead <- getLead
      engageEnemy lead attrs.id
      pure e
    _ -> GollumTheLandOfShadow <$> runMessage msg attrs
