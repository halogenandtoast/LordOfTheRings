module LordOfTheRings.Enemy.Cards.TheBlackSerpentCrossingsOfPoros (theBlackSerpentCrossingsOfPoros) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))

newtype TheBlackSerpentCrossingsOfPoros = TheBlackSerpentCrossingsOfPoros EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Crossings of Poros' The Black Serpent, added to the staging area at setup.
theBlackSerpentCrossingsOfPoros :: EnemyCard TheBlackSerpentCrossingsOfPoros
theBlackSerpentCrossingsOfPoros = enemy TheBlackSerpentCrossingsOfPoros Cards.theBlackSerpentCrossingsOfPoros

-- | Cannot leave the staging area or take damage, so no player is offered to engage it.
-- X is the stage number of the main quest. The Black Serpent gets +X threat, +X attack and
-- +X defense.
instance HasModifiersFor TheBlackSerpentCrossingsOfPoros where
  getModifiersFor (TheBlackSerpentCrossingsOfPoros a) = do
    x <- getMainStage
    modifySelf a [CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer, CannotTakeDamage, ThreatModifier x, AttackModifier x, DefenseModifier x]

instance HasAbilities TheBlackSerpentCrossingsOfPoros where
  getAbilities (TheBlackSerpentCrossingsOfPoros a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase]

-- | Forced: At the end of the encounter phase, either remove X progress from the main
-- quest, or The Black Serpent makes an attack against the first player. The removal is
-- offered only when it can be made in full (FAQ 1.44); the first player decides.
instance RunMessage TheBlackSerpentCrossingsOfPoros where
  runMessage msg e@(TheBlackSerpentCrossingsOfPoros attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- getMainStage
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theBlackSerpentCrossingsOfPoros.choose"
        causedBy attrs
        removeMainQuestProgressLabeled attrs x
        labeled "theBlackSerpentCrossingsOfPoros.attack" $ immediateAttack lead attrs.id
      pure e
    _ -> TheBlackSerpentCrossingsOfPoros <$> runMessage msg attrs
