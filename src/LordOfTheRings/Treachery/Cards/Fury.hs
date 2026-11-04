module LordOfTheRings.Treachery.Cards.Fury (fury) where

import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyHitPoints)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition, Easterling))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Fury = Fury TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

fury :: TreacheryCard Fury
fury = treachery Fury Cards.fury

{- | While attached to an enemy, counts as a Condition attachment with the text: "Forced:
When attached enemy would be destroyed, heal all damage from it and discard Fury instead."
Damage does not destroy the enemy while Fury is on it; once its damage reaches its hit
points, the damage is healed and Fury discarded, as Corsair Duelist's resource is spent.
-}
instance HasModifiersFor Fury where
  getModifiersFor (Fury a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Condition]
      modified a host [GainKeyword Indestructible]
    _ -> pure ()

instance HasAbilities Fury where
  getAbilities (Fury a) = [mkAbility a 1 $ silent $ DealtDamageWindow After host | AttachedTo host@(EnemyTarget _) <- [a.placement]]

-- | Surge.
-- When Revealed: Attach to the lowest engagement cost Easterling enemy without Fury.
instance RunMessage Fury where
  runMessage msg t@(Fury attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      void $ attachToEnemyWith attrs attrs.treacheryId attrs.treacheryCardCode (EnemyWithTrait Easterling) Lowest getEngagementCost (const $ pure ())
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ | AttachedTo (EnemyTarget eid) <- attrs.placement -> do
      enemy <- getEnemy eid
      hp <- getEnemyHitPoints eid
      when (enemy.damage >= hp) do
        healAllDamage attrs eid
        discard attrs
      pure t
    _ -> Fury <$> runMessage msg attrs
