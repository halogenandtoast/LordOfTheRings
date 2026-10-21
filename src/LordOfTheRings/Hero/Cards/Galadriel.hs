module LordOfTheRings.Hero.Cards.Galadriel (galadriel) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Galadriel = Galadriel HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

galadriel :: HeroCard Galadriel
galadriel = hero Galadriel Cards.galadriel

-- | Galadriel cannot quest, attack, or defend. Allies you control do not exhaust to commit
-- to the quest during the round they enter play.
instance HasModifiersFor Galadriel where
  getModifiersFor (Galadriel a) = do
    modifySelf a [CannotCommitToQuest, CannotAttackOrDefend]
    fresh <- select (AllyControlledBy (PlayerWithId a.owner) <> AllyEnteredPlayThisRound)
    for_ fresh \ally -> modified a ally [DoesNotExhaustToCommit]

-- | Action: Exhaust Galadriel to choose a player. That player reduces his threat by 1 and
-- draws 1 card. (Limit once per round.)
instance HasAbilities Galadriel where
  getAbilities (Galadriel a) = [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage Galadriel where
  runMessage msg h@(Galadriel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      choosePlayerM pid attrs \p -> do
        lowerThreat p 1
        drawCards p 1
      pure h
    _ -> Galadriel <$> runMessage msg attrs
