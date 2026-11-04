module LordOfTheRings.Location.Cards.PlainOfGorgoroth (plainOfGorgoroth) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PlainOfGorgoroth = PlainOfGorgoroth LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

plainOfGorgoroth :: LocationCard PlainOfGorgoroth
plainOfGorgoroth = location PlainOfGorgoroth Cards.plainOfGorgoroth

-- | If a player has failed a Fortitude test this round, Plain of Gorgoroth gets +2 threat.
instance HasModifiersFor PlainOfGorgoroth where
  getModifiersFor (PlainOfGorgoroth a) = do
    failed <- anyM failedFortitudeTestThisRound =<< getRemainingPlayers
    when failed $ modifySelf a [ThreatModifier 2]

-- | Travel: The first player makes a Fortitude test to travel here. If this test is
-- unsuccessful, raise each player's threat by 4.
instance RunMessage PlainOfGorgoroth where
  runMessage msg (PlainOfGorgoroth a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      fortitudeTest lead a (eachPlayer \pid -> encounterRaisesThreat pid 4) (pure ())
      PlainOfGorgoroth <$> runMessage msg a
    _ -> PlainOfGorgoroth <$> runMessage msg a
