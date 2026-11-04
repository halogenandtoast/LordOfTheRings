module LordOfTheRings.Hero.Cards.BilboBagginsOverHillAndUnderHill (bilboBagginsOverHillAndUnderHill) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype BilboBagginsOverHillAndUnderHill = BilboBagginsOverHillAndUnderHill HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Bilbo Baggins: he follows the first player token, by
-- the keyword on his card def, from the scenario's setup (see 'setUpFellowship').
bilboBagginsOverHillAndUnderHill :: HeroCard BilboBagginsOverHillAndUnderHill
bilboBagginsOverHillAndUnderHill = hero BilboBagginsOverHillAndUnderHill Cards.bilboBagginsOverHillAndUnderHill

-- | Bilbo Baggins cannot gain resources from player card effects.
instance HasModifiersFor BilboBagginsOverHillAndUnderHill where
  getModifiersFor (BilboBagginsOverHillAndUnderHill a) = modifySelf a [CannotGainResourcesFromPlayerCardEffects]

-- | If Bilbo Baggins leaves play, the players lose the game (The Hobbit: Over Hill and
-- Under Hill rules insert).
instance RunMessage BilboBagginsOverHillAndUnderHill where
  runMessage msg (BilboBagginsOverHillAndUnderHill attrs) = case msg of
    _ | leavesPlay attrs msg -> do
      gameOver Lost
      BilboBagginsOverHillAndUnderHill <$> runMessage msg attrs
    _ -> BilboBagginsOverHillAndUnderHill <$> runMessage msg attrs
