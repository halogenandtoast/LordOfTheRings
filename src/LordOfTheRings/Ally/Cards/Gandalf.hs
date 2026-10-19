module LordOfTheRings.Ally.Cards.Gandalf (gandalf) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Gandalf = Gandalf AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gandalf :: AllyCard Gandalf
gandalf = ally Gandalf Cards.gandalf

-- | Response: After Gandalf enters play, choose 1: draw 3 cards, deal 4 damage to 1 enemy
-- in play, or reduce your threat by 5.
instance HasAbilities Gandalf where
  getAbilities (Gandalf a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage Gandalf where
  runMessage msg g@(Gandalf attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      enemies <- select AnyEnemy
      let damage =
            questionLabelWith "Deal 4 damage to an enemy" (Just $ toTarget attrs) [] p
              [targetLabel e [DealDamage (toSource attrs) (toTarget e) 4] | e <- enemies]
      push
        $ questionLabelWith "Gandalf: choose 1" (Just $ toTarget attrs) [] p
        $ [Label "Draw 3 cards" [DrawCards $ Drawing p 3]]
        <> [Label "Deal 4 damage to 1 enemy in play" [damage] | notNull enemies]
        <> [Label "Reduce your threat by 5" [LowerThreat p 5]]
      pure g
    -- At the end of the round, discard Gandalf from play.
    EndRound -> do
      push $ Discard (toTarget attrs)
      pure g
    _ -> Gandalf <$> runMessage msg attrs
