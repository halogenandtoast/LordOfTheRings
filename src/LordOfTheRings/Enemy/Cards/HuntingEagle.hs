module LordOfTheRings.Enemy.Cards.HuntingEagle (huntingEagle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Creature))

newtype HuntingEagle = HuntingEagle EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingEagle :: EnemyCard HuntingEagle
huntingEagle = enemy HuntingEagle Cards.huntingEagle

-- | Surge. Cannot be optionally engaged.
instance HasModifiersFor HuntingEagle where
  getModifiersFor (HuntingEagle a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: When the encounter phase begins, Hunting Eagle engages the first player.
instance HasAbilities HuntingEagle where
  getAbilities (HuntingEagle a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if attacking enemy has the
-- Creature trait).
instance RunMessage HuntingEagle where
  runMessage msg e@(HuntingEagle attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      unless (attrs.placement == EngagedWith lead) $ engageEnemy lead attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      creature <- selectAny (EnemyWithId eid <> EnemyWithTrait Creature)
      enemyGetsAttack (CardIdSource cid) eid (if creature then 2 else 1)
      pure e
    _ -> HuntingEagle <$> runMessage msg attrs
