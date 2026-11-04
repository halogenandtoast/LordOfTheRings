module LordOfTheRings.Enemy.Cards.BillFerny (billFerny) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (eitherRingwraithOr)
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype BillFerny = BillFerny EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

billFerny :: EnemyCard BillFerny
billFerny = enemy BillFerny Cards.billFerny

-- | Players cannot optionally engage Bill Ferny.
instance HasModifiersFor BillFerny where
  getModifiersFor (BillFerny a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: At the beginning of the staging step, either shuffle 1 out of play Ringwraith
-- into the encounter deck or each player raises his threat by 1 for each non-Fellowship
-- hero he committed to the quest.
instance HasAbilities BillFerny where
  getAbilities (BillFerny a) = [mkAbility a 1 $ forced StagingStepBeginsWindow]

-- | The first player chooses. The heroes counted are those still committed as the staging
-- step begins.
instance RunMessage BillFerny where
  runMessage msg e@(BillFerny attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      eitherRingwraithOr lead attrs "billFerny.choose" True "billFerny.raiseThreat" do
        eachPlayer \p -> do
          n <- length <$> select (CommittedHero <> HeroControlledBy (PlayerWithId p) <> not_ (HeroWithSphere Fellowship))
          encounterRaisesThreat p n
      pure e
    _ -> BillFerny <$> runMessage msg attrs
