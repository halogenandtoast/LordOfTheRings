module LordOfTheRings.Enemy.Cards.DeadLord (deadLord) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (encounterDiscardPile)
import LordOfTheRings.Trait (Trait (Undead))

newtype DeadLord = DeadLord EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadLord :: EnemyCard DeadLord
deadLord = enemy DeadLord Cards.deadLord

-- | Forced: After Dead Lord engages you, put the topmost Undead enemy in the discard pile
-- into play engaged with you.
instance HasAbilities DeadLord where
  getAbilities (DeadLord a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, reveal 1
-- encounter card.
instance RunMessage DeadLord where
  runMessage msg e@(DeadLord attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      discards <- encounterDiscardPile
      let undead c = cdCardType (toCardDef c) == EnemyType && Undead `member` cdCardTraits (toCardDef c)
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid ->
        for_ (find undead discards) \card -> do
          removeFromEncounterDiscard card
          putEnemyIntoPlayEngaged pid card
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      revealEncounterCard
      pure e
    _ -> DeadLord <$> runMessage msg attrs
