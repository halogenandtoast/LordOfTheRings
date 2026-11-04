module LordOfTheRings.Objective.Cards.TrollKey (trollKey) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (runTrollTreasure, trollTreasureAbilities)

newtype TrollKey = TrollKey ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollKey :: ObjectiveCard TrollKey
trollKey = objective TrollKey Cards.trollKey

{- | If Troll Key is discarded, add it to the staging area.
If Troll Key is unattached and in the staging area, attach it to a Troll enemy, if able.
Response: After attached Troll enemy takes damage as the result of an attack, the first
player may exhaust Bilbo Baggins to claim this objective and attach it to him.
-}
instance HasAbilities TrollKey where
  getAbilities (TrollKey a) =
    trollTreasureAbilities a
      <> [ anyPlayer
             $ restricted a 2 (PlayerExists $ ActivePlayer <> FirstPlayer)
             $ ResponseAbility (PlayerAttackDealtDamageToWindow After AnyPlayer (EnemyWithId eid)) (ExhaustHeroCost bilbo)
         | AttachedTo (EnemyTarget eid) <- [a.placement]
         ]
   where
    bilbo = HeroWithTitle "Bilbo Baggins" <> HeroControlledBy ActivePlayer <> ReadyHero

instance RunMessage TrollKey where
  runMessage msg o@(TrollKey attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      traverse_ (attachObjective attrs.id) =<< getBilbo
      pure o
    _ -> TrollKey <$> runTrollTreasure msg attrs
