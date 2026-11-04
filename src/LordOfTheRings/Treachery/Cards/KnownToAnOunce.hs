module LordOfTheRings.Treachery.Cards.KnownToAnOunce (knownToAnOunce) where

import LordOfTheRings.Card (CardDef (cdCardType), CardType (EventType), toCardDef)
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype KnownToAnOunce = KnownToAnOunce TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Its burgle effect is read by burgle attempts only.
knownToAnOunce :: TreacheryCard KnownToAnOunce
knownToAnOunce = treachery KnownToAnOunce Cards.knownToAnOunce

{- | When Revealed: Each player discards 2 random cards from hand. Until the end of the
phase, each Dragon enemy in play gets +1 threat for each event discarded by this effect.
The cards are picked at random here, to count the events among them.
-}
instance RunMessage KnownToAnOunce where
  runMessage msg t@(KnownToAnOunce attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discarded <- for players \pid -> do
        hand <- (.hand) <$> getPlayer pid
        picked <- take 2 <$> shuffleM hand
        for_ picked (discardFromHand pid)
        pure picked
      let events = length [c | c <- concat discarded, cdCardType (toCardDef c) == EventType]
      dragons <- select (EnemyWithTrait Dragon)
      when (events > 0) $ for_ dragons \d -> phaseModifier attrs d (ThreatModifier events)
      pure t
    _ -> KnownToAnOunce <$> runMessage msg attrs
