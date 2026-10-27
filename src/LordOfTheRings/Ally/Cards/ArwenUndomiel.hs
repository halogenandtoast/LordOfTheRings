module LordOfTheRings.Ally.Cards.ArwenUndomiel (arwenUndomiel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype ArwenUndomiel = ArwenUndomiel AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

arwenUndomiel :: AllyCard ArwenUndomiel
arwenUndomiel = ally ArwenUndomiel Cards.arwenUndomiel

-- | Response: After Arwen Undómiel exhausts, choose a character. That character gains
-- sentinel and gets +1 defense until the end of the round.
instance HasAbilities ArwenUndomiel where
  getAbilities (ArwenUndomiel a) = [mkAbility a 1 $ response $ ExhaustedWindow After (toTarget a)]

instance RunMessage ArwenUndomiel where
  runMessage msg s@(ArwenUndomiel a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      characters <- selectCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "arwenUndomiel.choose"
        causedBy a
        targets characters \c -> roundModifiers a c [GainKeyword Sentinel, DefenseModifier 1]
      pure s
    _ -> ArwenUndomiel <$> runMessage msg a
