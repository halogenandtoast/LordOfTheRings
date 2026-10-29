module LordOfTheRings.Enemy.Cards.OrcSkirmisher (orcSkirmisher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcSkirmisher = OrcSkirmisher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcSkirmisher :: EnemyCard OrcSkirmisher
orcSkirmisher = enemy OrcSkirmisher Cards.orcSkirmisher

-- | Forced: After Orc Skirmisher engages a player, that player must either deal 3 damage to
-- a character he controls, or remove 1 time counter from the current quest.
instance HasAbilities OrcSkirmisher where
  getAbilities (OrcSkirmisher a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack.
instance RunMessage OrcSkirmisher where
  runMessage msg e@(OrcSkirmisher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        characters <- charactersControlledBy pid
        timed <- canRemoveTimeCounterFromCurrentQuest
        -- either one may be chosen, but one that can be done in full must be (FAQ 1.44)
        chooseOrRunOneM pid do
          questionLabeled "orcSkirmisher.choose"
          causedBy attrs
          when (notNull characters) $ labeled "orcSkirmisher.damage" $ dealDamageToOneCharacter attrs pid 3
          when timed $ labeled "removeTimeCounterFromQuest" $ removeTimeCountersFromCurrentQuest attrs 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure e
    _ -> OrcSkirmisher <$> runMessage msg attrs
