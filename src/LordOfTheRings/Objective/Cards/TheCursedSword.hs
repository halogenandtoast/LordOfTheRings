module LordOfTheRings.Objective.Cards.TheCursedSword (theCursedSword) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

-- | Cannot be discarded by player card effects. Attached to Captain Sahír as he enters play
-- (The Haven of Umbar sets it aside with him, in nightmare mode).
newtype TheCursedSword = TheCursedSword ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCursedSword :: ObjectiveCard TheCursedSword
theCursedSword = objective TheCursedSword Cards.theCursedSword

-- | The Cursed Sword counts as a Weapon attachment with the text: "Forced: After Captain
-- Sahír attacks and deals excess damage, assign that excess damage to Captain Sahír as
-- resources. (Limit once per round.)"
instance HasAbilities TheCursedSword where
  getAbilities (TheCursedSword a) =
    [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ forced $ EnemyDealtExcessDamageWindow After eid
    | AttachedTo (EnemyTarget eid) <- [a.objectivePlacement]
    ]

instance RunMessage TheCursedSword where
  runMessage msg o@(TheCursedSword attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [(eid, n) | Window _ (AttackDealtExcessDamage eid _ n) <- windows] \(eid, n) -> placeResources attrs eid n
      pure o
    _ -> TheCursedSword <$> runMessage msg attrs
