module LordOfTheRings.Enemy.Cards.ServantOfSauron (servantOfSauron) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

newtype ServantOfSauron = ServantOfSauron EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

servantOfSauron :: EnemyCard ServantOfSauron
servantOfSauron = enemy ServantOfSauron Cards.servantOfSauron

-- | Surge.
-- Servant of Sauron engages the first player: no other player can engage it.
instance HasModifiersFor ServantOfSauron where
  getModifiersFor (ServantOfSauron a) = do
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    modifySelf a $ EngagesOnly lead : map CannotEngagePlayer others

-- | Forced: After the engaged player raises his threat, Servant of Sauron makes an
-- immediate attack. (Limit once per phase.)
instance HasAbilities ServantOfSauron where
  getAbilities (ServantOfSauron a) = case a.placement of
    EngagedWith pid -> [limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ forced $ ThreatRaisedWindow After (PlayerWithId pid)]
    _ -> []

{- | It engages the first player as it enters play, and again whenever it would return to
the staging area, as Gollum does under the same words (MEC77 rules); it follows the first
player token as it passes.
-}
instance RunMessage ServantOfSauron where
  runMessage msg e@(ServantOfSauron attrs) = case msg of
    CheckWindows ws | notNull [() | Window After (EnemyEnteredPlay eid) <- ws, eid == attrs.id] -> do
      lead <- getLead
      when (attrs.placement /= EngagedWith lead) $ engageEnemy lead attrs.id
      ServantOfSauron <$> runMessage msg attrs
    SetFirstPlayer pid | attrs.placement /= EngagedWith pid -> do
      engageEnemy pid attrs.id
      ServantOfSauron <$> runMessage msg attrs
    PlaceEnemy eid StagingArea | eid == attrs.id -> do
      lead <- getLead
      engageEnemy lead attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      case attrs.placement of
        EngagedWith pid -> immediateAttack pid attrs.id
        _ -> pure ()
      pure e
    _ -> ServantOfSauron <$> runMessage msg attrs
