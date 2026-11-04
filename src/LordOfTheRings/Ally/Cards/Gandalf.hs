{-# LANGUAGE RankNTypes #-}

module LordOfTheRings.Ally.Cards.Gandalf (gandalf) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message.Lifted.UpTo

newtype Gandalf = Gandalf AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gandalf :: AllyCard Gandalf
gandalf = ally Gandalf Cards.gandalf

-- | Response: After Gandalf enters play, choose 1: draw 3 cards, deal 4 damage to 1 enemy
-- in play, or reduce your threat by 5. (Up to more of them, as an upgraded Send for Help
-- lets him choose.)
instance HasAbilities Gandalf where
  getAbilities (Gandalf a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage Gandalf where
  runMessage msg g@(Gandalf attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      enemies <- select AnyEnemy
      upTo <- maximumMay . (\ms -> [n | ChoosesUpToOptions n <- ms]) <$> getModifiers attrs
      let options =
            [UpToLabel "gandalf.draw" (drawCards attrs p 3)]
              <> [ UpToLabel "gandalf.damage" $ chooseOneM p do
                    questionLabeled "gandalf.chooseEnemy"
                    causedBy attrs
                    targets enemies \e -> dealDamage attrs e 4
                 | notNull enemies
                 ]
              <> [UpToLabel "gandalf.threat" (lowerThreatBy attrs p 5)]
      case upTo of
        Just n -> chooseUpToM p "gandalf.chooseUpTo" (toTarget attrs) [] n options
        Nothing -> chooseOneM p do
          questionLabeled "gandalf.chooseOne"
          causedBy attrs
          for_ options \case
            UpToLabel key act -> labeled key act
            UpToTarget target act -> targeting target act
      pure g
    -- At the end of the round, discard Gandalf from play.
    EndRound -> do
      discard attrs
      pure g
    _ -> Gandalf <$> runMessage msg attrs
