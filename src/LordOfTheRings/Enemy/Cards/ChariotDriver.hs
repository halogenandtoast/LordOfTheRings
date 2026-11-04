module LordOfTheRings.Enemy.Cards.ChariotDriver (chariotDriver) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (placeWainridersDamage)

newtype ChariotDriver = ChariotDriver EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chariotDriver :: EnemyCard ChariotDriver
chariotDriver = enemy ChariotDriver Cards.chariotDriver

-- | When Revealed: Either place 3 damage on The Wainriders' stage, or Chariot Driver makes an
-- attack against the first player. The first player chooses.
-- Shadow: If this attack destroys a character, return attacking enemy to the staging area.
instance RunMessage ChariotDriver where
  runMessage msg e@(ChariotDriver attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "chariotDriver.choose"
        causedBy attrs
        labeled "chariotDriver.damage" $ placeWainridersDamage 3
        labeled "chariotDriver.attack" $ immediateAttack lead attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      stillHere <- selectAny (EnemyWithId eid <> not_ EnemyInStagingArea)
      when stillHere $ placeEnemy eid StagingArea
      pure e
    _ -> ChariotDriver <$> runMessage msg attrs
