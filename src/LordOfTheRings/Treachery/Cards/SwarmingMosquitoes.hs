module LordOfTheRings.Treachery.Cards.SwarmingMosquitoes (swarmingMosquitoes) where

import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SwarmingMosquitoes = SwarmingMosquitoes TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swarmingMosquitoes :: TreacheryCard SwarmingMosquitoes
swarmingMosquitoes = treachery SwarmingMosquitoes Cards.swarmingMosquitoes

-- | When Revealed: The first player makes an escape test counting defense instead of
-- willpower, dealing 2 cards from the encounter deck. If this test is failed, place 1
-- resource token on Gollum and deal 1 damage to each character in play.
instance RunMessage SwarmingMosquitoes where
  runMessage msg t@(SwarmingMosquitoes attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      escapeTest lead attrs 2 EscapeDefense (placeOnGollum attrs 1 >> for_ characters \c -> dealDamage attrs c 1) (pure ())
      pure t
    _ -> SwarmingMosquitoes <$> runMessage msg attrs
