module LordOfTheRings.Treachery.Cards.FellBeast (fellBeast) where

import LordOfTheRings.Placement (Placement (AttachedTo, EngagedWith, StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (attachedCopies)
import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FellBeast = FellBeast TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
fellBeast :: TreacheryCard FellBeast
fellBeast = treachery FellBeast Cards.fellBeast

-- | Counts as a Mount attachment with the text: "Attached enemy gets +1 threat, +1 attack,
-- +1 defense and -10 engagement cost."
instance HasModifiersFor FellBeast where
  getModifiersFor (FellBeast a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1, EngagementCostModifier (-10)]
    _ -> pure ()

-- | "Forced: At the end of the round, return attached enemy to the staging area."
instance HasAbilities FellBeast where
  getAbilities (FellBeast a) = [mkAbility a 1 $ forced RoundEndsWindow | AttachedTo (EnemyTarget _) <- [a.placement]]

-- | When Revealed: Attach to a Nazgûl enemy. Limit 1 per enemy. The first player chooses
-- among those without one; with none, it has nothing to attach to.
instance RunMessage FellBeast where
  runMessage msg t@(FellBeast attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      nazgul <- filterM (fmap (== 0) . attachedCopies Cards.fellBeast) =<< select (EnemyWithTrait Nazgul)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "fellBeast.attach"
        causedBy attrs
        targets nazgul $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [eid | AttachedTo (EnemyTarget eid) <- [attrs.placement]] \eid -> do
        engaged <- (\e -> case e.placement of EngagedWith _ -> True; _ -> False) <$> getEnemy eid
        when engaged $ placeEnemy eid StagingArea
      pure t
    _ -> FellBeast <$> runMessage msg attrs
