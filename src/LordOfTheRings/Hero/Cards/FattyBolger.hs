module LordOfTheRings.Hero.Cards.FattyBolger (fattyBolger) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FattyBolger = FattyBolger HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fattyBolger :: HeroCard FattyBolger
fattyBolger = hero FattyBolger Cards.fattyBolger

-- | Action: Exhaust Fatty Bolger to choose an enemy in the staging area and raise your
-- threat by that enemy's [threat]. Until the end of the phase, that enemy does not
-- contribute its [threat]. (Limit once per round.)
instance HasAbilities FattyBolger where
  getAbilities (FattyBolger a) =
    [ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (exists EnemyInStagingArea)
        $ ActionAbility (exhaustCost a)
    ]

instance RunMessage FattyBolger where
  runMessage msg h@(FattyBolger attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      chooseOneM pid do
        questionLabeled "fattyBolger.chooseEnemy"
        causedBy attrs
        targets enemies \eid -> do
          raiseThreat pid =<< getEnemyThreat eid
          phaseModifier attrs eid DoesNotContributeThreat
      pure h
    _ -> FattyBolger <$> runMessage msg attrs
