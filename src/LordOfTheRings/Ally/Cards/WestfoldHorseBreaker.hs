module LordOfTheRings.Ally.Cards.WestfoldHorseBreaker (westfoldHorseBreaker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype WestfoldHorseBreaker = WestfoldHorseBreaker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westfoldHorseBreaker :: AllyCard WestfoldHorseBreaker
westfoldHorseBreaker = ally WestfoldHorseBreaker Cards.westfoldHorseBreaker

-- | Action: Discard Westfold Horse-breaker to choose and ready a hero.
instance HasAbilities WestfoldHorseBreaker where
  getAbilities (WestfoldHorseBreaker a) = [restricted a 1 (exists ExhaustedHero) $ ActionAbility Free]

instance RunMessage WestfoldHorseBreaker where
  runMessage msg s@(WestfoldHorseBreaker a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select ExhaustedHero
      chooseOneM pid do
        questionLabeled "westfoldHorseBreaker.ready"
        causedBy a
        targets heroes ready
      -- the discard is the cost, but what it leads to waits for the effect
      discard a
      pure s
    _ -> WestfoldHorseBreaker <$> runMessage msg a
