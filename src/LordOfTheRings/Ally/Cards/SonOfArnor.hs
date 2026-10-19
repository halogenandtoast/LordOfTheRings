module LordOfTheRings.Ally.Cards.SonOfArnor (sonOfArnor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype SonOfArnor = SonOfArnor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sonOfArnor :: AllyCard SonOfArnor
sonOfArnor = ally SonOfArnor Cards.sonOfArnor

-- | Response: After Son of Arnor enters play, choose an enemy card in the staging area
-- and engage it.
instance HasAbilities SonOfArnor where
  getAbilities (SonOfArnor a) =
    [restricted a 1 (exists EnemyInStagingArea) $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage SonOfArnor where
  runMessage msg s@(SonOfArnor attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      push
        $ questionLabelWith "Choose an enemy to engage" (Just $ toTarget attrs) [] p
          [targetLabel e [EngageEnemy p e] | e <- enemies]
      pure s
    _ -> SonOfArnor <$> runMessage msg attrs
