module LordOfTheRings.Location.Cards.TheHighPass (theHighPass) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheHighPass = TheHighPass LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theHighPass :: LocationCard TheHighPass
theHighPass = location TheHighPass Cards.theHighPass

-- | X is the number of players in the game.
instance HasModifiersFor TheHighPass where
  getModifiersFor (TheHighPass a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ThreatModifier players]

-- | Forced: At the end of the round, remove X progress tokens from the current quest.
instance HasAbilities TheHighPass where
  getAbilities (TheHighPass a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage TheHighPass where
  runMessage msg l@(TheHighPass a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeProgressFromQuest a . length =<< getRemainingPlayers
      pure l
    _ -> TheHighPass <$> runMessage msg a
