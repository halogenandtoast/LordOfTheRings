module LordOfTheRings.Ally.Cards.MarksmanOfLorien (marksmanOfLorien) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype MarksmanOfLorien = MarksmanOfLorien AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, as its card def says.
marksmanOfLorien :: AllyCard MarksmanOfLorien
marksmanOfLorien = ally MarksmanOfLorien Cards.marksmanOfLorien

-- | Response: After Marksman of Lórien enters play, choose an enemy. That enemy gets -2
-- defense until the end of the round.
instance HasAbilities MarksmanOfLorien where
  getAbilities (MarksmanOfLorien a) =
    [restricted a 1 (exists AnyEnemy) $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage MarksmanOfLorien where
  runMessage msg s@(MarksmanOfLorien a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select AnyEnemy
      chooseOneM pid do
        questionLabeled "marksmanOfLorien.enemy"
        causedBy a
        targets enemies \e -> roundModifier a e (DefenseModifier (-2))
      pure s
    _ -> MarksmanOfLorien <$> runMessage msg a
