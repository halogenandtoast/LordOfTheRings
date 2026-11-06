module LordOfTheRings.Treachery.Cards.OutOfTheMist (outOfTheMist) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OutOfTheMist = OutOfTheMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

outOfTheMist :: TreacheryCard OutOfTheMist
outOfTheMist = treachery OutOfTheMist Cards.outOfTheMist

attackState :: TreacheryAttrs -> Maybe (EnemyId, Bool)
attackState = parseMaybe parseJSON . treacheryMeta

instance RunMessage OutOfTheMist where
  runMessage msg t@(OutOfTheMist attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse (\eid -> (eid,) <$> getEngagementCost eid) =<< select AnyEnemy
      let highest = maximumMay $ map snd enemies
      lead <- getLead
      if null enemies
        then revealEncounterCard
        else chooseOrRunOneM lead do
          questionLabeled "dreadnaught.outOfTheMist"
          causedBy attrs
          targets [eid | (eid, n) <- enemies, Just n == highest] \eid ->
            push $ WithTarget (toTarget eid) $ DoStep 1 msg
      pure t
    WithTarget (EnemyTarget eid) (DoStep 1 inner@(WhenRevealed (isTarget attrs -> True))) -> do
      lead <- getLead
      push $ EngageEnemy lead eid
      doStep 2 inner
      pure $ OutOfTheMist attrs {treacheryMeta = toJSON (eid, False)}
    DoStep 2 inner@(WhenRevealed (isTarget attrs -> True)) -> do
      for_ (attackState attrs) \(eid, _) -> do
        alive <- selectAny $ EnemyWithId eid
        lead <- getLead
        mods <- if alive then getModifiers eid else pure [CannotAttack]
        unless (CannotAttack `elem` mods || CannotAttackPlayer lead `elem` mods) $ immediateAttack lead eid
      doStep 3 inner
      pure t
    -- Only a completed attack counts. A cancelled attack opens no After window;
    -- using "attack initiated" would incorrectly suppress surge after cancellation.
    CheckWindows ws | Just (eid, _) <- attackState attrs -> do
      let made = any (\case Window After (EnemyAttacking attacking _) -> attacking == eid; _ -> False) ws
      pure $ if made then OutOfTheMist attrs {treacheryMeta = toJSON (eid, True)} else t
    DoStep 3 (WhenRevealed (isTarget attrs -> True)) -> do
      unless (maybe False snd $ attackState attrs) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "dreadnaught.outOfTheMistShadow"
        causedBy (CardIdTarget cid)
        labeled "dreadnaught.raiseFour" $ encounterRaisesThreat pid 4
        labeled "dreadnaught.additionalAttack" $ additionalAttack pid eid
      pure t
    _ -> OutOfTheMist <$> runMessage msg attrs
