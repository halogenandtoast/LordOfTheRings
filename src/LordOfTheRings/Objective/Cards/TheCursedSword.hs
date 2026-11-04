module LordOfTheRings.Objective.Cards.TheCursedSword (theCursedSword) where

import LordOfTheRings.Helpers.Character (getDefenseAgainst, getHitPoints)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

-- | Cannot be discarded by player card effects. Attached to Captain Sahír as he enters play
-- (The Haven of Umbar sets it aside with him, in nightmare mode).
newtype TheCursedSword = TheCursedSword ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCursedSword :: ObjectiveCard TheCursedSword
theCursedSword = objective TheCursedSword Cards.theCursedSword

-- | The Cursed Sword counts as a Weapon attachment with the text: "Forced: After Captain
-- Sahír attacks and deals excess damage, assign that excess damage to Captain Sahír as
-- resources. (Limit once per round.)"
instance RunMessage TheCursedSword where
  runMessage msg (TheCursedSword attrs) = case msg of
    AttackDamage eid defender
      | AttachedTo (EnemyTarget host) <- attrs.objectivePlacement
      , host == eid
      , character defender -> do
          attack <- getEnemyAttack eid
          defense <- getDefenseAgainst eid defender
          remaining <- (-) <$> getHitPoints defender <*> getDamage defender
          let excess = attack - defense - remaining
          when (excess > 0) $ placeResources attrs eid excess
          TheCursedSword <$> runMessage msg attrs
    _ -> TheCursedSword <$> runMessage msg attrs
   where
    character = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
