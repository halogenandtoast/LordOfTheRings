module LordOfTheRings.Enemy.Cards.OrcCaptain (orcCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OrcCaptain = OrcCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcCaptain :: EnemyCard OrcCaptain
orcCaptain = enemy OrcCaptain Cards.orcCaptain

-- | Forced: After Orc Captain engages you, make a Fortitude test. If you fail this test,
-- reveal the top card of the encounter deck.
instance HasAbilities OrcCaptain where
  getAbilities (OrcCaptain a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: If this attack destroys a character, return attacking enemy to the staging
-- area.
instance RunMessage OrcCaptain where
  runMessage msg e@(OrcCaptain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        fortitudeTest pid attrs (playerRevealsEncounterCard pid) (pure ())
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      stillHere <- selectAny (EnemyWithId eid <> not_ EnemyInStagingArea)
      when stillHere $ placeEnemy eid StagingArea
      pure e
    _ -> OrcCaptain <$> runMessage msg attrs
