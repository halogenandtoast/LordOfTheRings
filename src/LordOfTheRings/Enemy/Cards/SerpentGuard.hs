module LordOfTheRings.Enemy.Cards.SerpentGuard (serpentGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SerpentGuard = SerpentGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

serpentGuard :: EnemyCard SerpentGuard
serpentGuard = enemy SerpentGuard Cards.serpentGuard

-- | Forced: After Serpent Guard engages you, either remove 2 progress from the main quest,
-- or enemies engaged with you cannot take damage until the end of the round. The removal
-- is offered only when it can be made in full (FAQ 1.44).
instance HasAbilities SerpentGuard where
  getAbilities (SerpentGuard a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Either remove 2 progress from main quest, or attacking enemy cannot take damage
-- this round.
instance RunMessage SerpentGuard where
  runMessage msg e@(SerpentGuard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "serpentGuard.choose"
          causedBy attrs
          removeMainQuestProgressLabeled attrs 2
          labeled "serpentGuard.noDamage" do
            enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
            for_ enemies \en -> roundModifier attrs en CannotTakeDamage
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseOrRunOneM pid do
        questionLabeled "serpentGuard.choose"
        causedBy attrs
        removeMainQuestProgressLabeled attrs 2
        labeled "serpentGuard.attackerNoDamage" $ roundModifier attrs eid CannotTakeDamage
      pure e
    _ -> SerpentGuard <$> runMessage msg attrs
