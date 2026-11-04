module LordOfTheRings.Location.Cards.Grond (grond) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (damageMinasTirith)

newtype Grond = Grond LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

grond :: LocationCard Grond
grond = location Grond Cards.grond

-- | Immune to player card effects. X is the number of resources here.
instance HasModifiersFor Grond where
  getModifiersFor (Grond a) = modifySelf a $ ImmuneToPlayerCardEffects : [ThreatModifier x | let x = countTokens Resource (locationTokens a), x > 0]

-- | Forced: At the beginning of the quest phase, place 1 resource on Grond. Then, deal X
-- damage to Minas Tirith.
instance HasAbilities Grond where
  getAbilities (Grond a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

-- | Travel: The Witch-king makes an immediate attack against each player, in turn order.
instance RunMessage Grond where
  runMessage msg l@(Grond a) = case msg of
    TravelTo lid | lid == a.id -> do
      witchKing <- select (EnemyWithCardCode $ toCardCode Enemies.theWitchKing)
      for_ witchKing \eid -> eachPlayer \p -> immediateAttack p eid
      Grond <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      damageMinasTirith a (countTokens Resource (locationTokens a))
      pure l
    _ -> Grond <$> runMessage msg a
