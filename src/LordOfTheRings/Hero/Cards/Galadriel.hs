module LordOfTheRings.Hero.Cards.Galadriel (galadriel) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Galadriel = Galadriel HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadriel :: HeroCard Galadriel
galadriel = hero Galadriel Cards.galadriel

-- | Galadriel cannot quest, attack, or defend.
instance HasModifiersFor Galadriel where
  getModifiersFor (Galadriel a) = modifySelf a [CannotCommitToQuest, CannotAttackOrDefend]

-- | Allies you control do not exhaust to commit to the quest during the round they enter
-- play. Action: Exhaust Galadriel to choose a player. That player reduces his threat by
-- 1 and draws 1 card. (Limit once per round.)
--
-- ponytail: each ally is marked as it enters play, so one that entered before Galadriel
-- did that round is not, and a marked ally stays marked should she leave play.
instance HasAbilities Galadriel where
  getAbilities (Galadriel a) =
    [ mkAbility a 1 $ silent $ AllyEnteredPlayWindow After (Just a.owner) Nothing
    , limited (GroupLimit PerRound 1) $ mkAbility a 2 $ ActionAbility (ExhaustCost $ toTarget a)
    ]

instance RunMessage Galadriel where
  runMessage msg h@(Galadriel attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [t | Window _ (AllyEnteredPlay t _ _) <- windows] \t -> roundModifier attrs t DoesNotExhaustToCommit
      pure h
    UseCardAbility pid (isSource attrs -> True) 2 _ -> do
      choosePlayerM pid attrs \p -> do
        lowerThreat p 1
        drawCards p 1
      pure h
    _ -> Galadriel <$> runMessage msg attrs
