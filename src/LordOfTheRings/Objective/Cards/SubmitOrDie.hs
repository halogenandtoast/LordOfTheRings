module LordOfTheRings.Objective.Cards.SubmitOrDie (submitOrDie) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype SubmitOrDie = SubmitOrDie ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

submitOrDie :: ObjectiveCard SubmitOrDie
submitOrDie = objective SubmitOrDie Cards.submitOrDie

-- | Each non-unique enemy gets +1 Attack.
instance HasModifiersFor SubmitOrDie where
  getModifiersFor (SubmitOrDie a) = do
    enemies <- select (not_ UniqueEnemy)
    for_ enemies \e -> modified a e [AttackModifier 1]

-- | Forced: After an enemy attacks and destroys a character, place 1 damage token here.
-- Then, if there are X damage tokens here, the players lose the game. X is 4 more than the
-- number of players.
instance HasAbilities SubmitOrDie where
  getAbilities (SubmitOrDie a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After Nothing]

instance RunMessage SubmitOrDie where
  runMessage msg o@(SubmitOrDie attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeTokens attrs attrs Damage 1
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      x <- (+ 4) . length <$> getRemainingPlayers
      when (countTokens Damage attrs.objectiveTokens >= x) $ gameOver Lost
      pure o
    _ -> SubmitOrDie <$> runMessage msg attrs
