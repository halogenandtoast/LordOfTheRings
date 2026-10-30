module LordOfTheRings.Hero.Cards.Argalad (argalad) where

import LordOfTheRings.Helpers.Character (getAttack)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Argalad = Argalad HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
argalad :: HeroCard Argalad
argalad = hero Argalad Cards.argalad

-- | Action: Exhaust Argalad to choose an enemy in the staging area. Until the end of the
-- phase, that enemy gets -X threat, where X is Argalad's attack. If this effect reduces the
-- enemy's threat to 0, deal 1 damage to it. (Limit once per round.)
instance HasAbilities Argalad where
  getAbilities (Argalad a) =
    [limited (GroupLimit PerRound 1) $ restricted a 1 (exists EnemyInStagingArea) $ ActionAbility (exhaustCost a)]

instance RunMessage Argalad where
  runMessage msg h@(Argalad attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      x <- getAttack (toTarget attrs)
      enemies <- select EnemyInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \e -> do
          threat <- getEnemyThreat e
          phaseModifier attrs e (ThreatModifier (negate x))
          when (threat > 0 && x >= threat) $ dealDamage attrs e 1
      pure h
    _ -> Argalad <$> runMessage msg attrs
