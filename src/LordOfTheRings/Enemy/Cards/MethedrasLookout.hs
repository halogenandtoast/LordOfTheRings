module LordOfTheRings.Enemy.Cards.MethedrasLookout (methedrasLookout) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MethedrasLookout = MethedrasLookout EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

methedrasLookout :: EnemyCard MethedrasLookout
methedrasLookout = enemy MethedrasLookout Cards.methedrasLookout

-- | While Methedras Lookout is in the staging area, it gains: "Forced: At the end of the
-- refresh phase, remove 1 time counter from the current quest."
instance HasAbilities MethedrasLookout where
  getAbilities (MethedrasLookout a) =
    [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.placement == StagingArea]

-- | Shadow: Either exhaust a character you control, or return attacking enemy to the
-- staging area.
instance RunMessage MethedrasLookout where
  runMessage msg e@(MethedrasLookout attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      characters <- exhaustableCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "methedrasLookout.choose"
        causedBy attrs
        unless (null characters) $ labeled "methedrasLookout.exhaust" $ chooseAndExhaust attrs pid 1
        -- ponytail: returned once the attack resolves, so the attack under way is not broken off
        labeled "methedrasLookout.return" $ returnToStagingAfterAttack eid
      pure e
    _ -> MethedrasLookout <$> runMessage msg attrs
