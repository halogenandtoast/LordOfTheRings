module LordOfTheRings.Enemy.Cards.TheBlackSerpent (theBlackSerpent) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype TheBlackSerpent = TheBlackSerpent EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Black Serpent's (17094); The Crossings of Poros has a card of its own.
theBlackSerpent :: EnemyCard TheBlackSerpent
theBlackSerpent = enemy TheBlackSerpent Cards.theBlackSerpent

{- | Cannot leave the staging area or take damage. X is the stage number of the main
quest. The Black Serpent gets +X threat, +X attack and +X defense.
While there are at least 6 progress tokens on Defeat the Serpent, it loses "Cannot leave
the staging area or take damage", and the players may engage it then.
-}
instance HasModifiersFor TheBlackSerpent where
  getModifiersFor (TheBlackSerpent a) = do
    x <- getMainStage
    loses <- getsGame (any (\q -> toCardCode q == toCardCode Quests.defeatTheSerpent && q.flipped && q.progress >= 6) . toList . (.entities.quests))
    modifySelf a
      $ [ThreatModifier x, AttackModifier x, DefenseModifier x]
      <> (if loses then [] else [CannotLeaveStagingArea, CannotTakeDamage, CannotBeOptionallyEngagedBy AnyPlayer])

-- | Forced: At the end of the encounter phase, either remove X progress from the main
-- quest, or The Black Serpent makes an immediate attack against the first player. The
-- removal is offered only when it can be made in full (FAQ 1.44).
instance HasAbilities TheBlackSerpent where
  getAbilities (TheBlackSerpent a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase]

instance RunMessage TheBlackSerpent where
  runMessage msg e@(TheBlackSerpent attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- getMainStage
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theBlackSerpent.choose"
        causedBy attrs
        removeMainQuestProgressLabeled attrs x
        labeled "theBlackSerpent.attack" $ immediateAttack lead attrs.id
      pure e
    _ -> TheBlackSerpent <$> runMessage msg attrs
