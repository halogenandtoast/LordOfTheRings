module LordOfTheRings.Hero.Cards.HaldirOfLorien (haldirOfLorien) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype HaldirOfLorien = HaldirOfLorien HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

haldirOfLorien :: HeroCard HaldirOfLorien
haldirOfLorien = hero HaldirOfLorien Cards.haldirOfLorien

notEngagedWith :: PlayerId -> EnemyMatcher
notEngagedWith = not_ . EnemyEngagedWith . PlayerWithId

-- | Ranged. Combat Action: If you have not engaged an enemy this round, exhaust Haldir of
-- Lórien to declare him as an attacker (and resolve his attack) against an enemy not
-- engaged with you. Limit once per round.
instance HasAbilities HaldirOfLorien where
  getAbilities (HaldirOfLorien a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted
          a
          1
          ( DuringPhase CombatPhase
              <> TargetLacksModifier (PlayerTarget a.owner) EngagedAnEnemyThisRound
              <> exists (notEngagedWith a.owner)
          )
        $ ActionAbility (ExhaustCost $ toTarget a)
    ]

instance RunMessage HaldirOfLorien where
  runMessage msg h@(HaldirOfLorien attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (notEngagedWith pid)
      chooseOneM pid do
        questionLabeled "chooseEnemyToAttack"
        causedBy attrs
        targets enemies \e -> attackEnemyWith pid attrs (toTarget attrs) e 0
      pure h
    _ -> HaldirOfLorien <$> runMessage msg attrs
