module LordOfTheRings.Enemy.Cards.PatrolLeader (patrolLeader) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype PatrolLeader = PatrolLeader EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

patrolLeader :: EnemyCard PatrolLeader
patrolLeader = enemy PatrolLeader Cards.patrolLeader

-- | Forced: Before Patrol Leader is dealt damage, discard the top card of the encounter
-- deck. If the discarded card is an enemy, cancel that damage.
instance HasAbilities PatrolLeader where
  getAbilities (PatrolLeader a) = [mkAbility a 1 $ forced $ DealtDamageWindow When (toTarget a)]

-- | Shadow: Cancel all damage dealt to this enemy.
instance RunMessage PatrolLeader where
  runMessage msg e@(PatrolLeader attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      when (any ((== EnemyType) . cdCardType . toCardDef) cards) $ cancelDamage attrs
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid CannotTakeDamage
      pure e
    _ -> PatrolLeader <$> runMessage msg attrs
