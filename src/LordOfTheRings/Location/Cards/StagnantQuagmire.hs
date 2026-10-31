module LordOfTheRings.Location.Cards.StagnantQuagmire (stagnantQuagmire) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype StagnantQuagmire = StagnantQuagmire LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stagnantQuagmire :: LocationCard StagnantQuagmire
stagnantQuagmire = location StagnantQuagmire Cards.stagnantQuagmire

-- | While Stagnant Quagmire is in play, it adds its Escape value to all escape tests.
instance HasModifiersFor StagnantQuagmire where
  getModifiersFor (StagnantQuagmire a) = modified a GameTarget [EscapeTestBonus 2]

-- | Shadow: Perform an escape test, dealing 1 card from the encounter deck. If this test is
-- failed, discard defending character.
instance RunMessage StagnantQuagmire where
  runMessage msg l@(StagnantQuagmire a) = case msg of
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      escapeTest pid a 1 EscapeWillpower (traverse_ discard defenders) (pure ())
      pure l
    _ -> StagnantQuagmire <$> runMessage msg a
