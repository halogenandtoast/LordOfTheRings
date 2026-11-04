module LordOfTheRings.Treachery.Cards.FearOfDiscovery (fearOfDiscovery) where

import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FearOfDiscovery = FearOfDiscovery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Flight to the Ford burden set. Surge.
fearOfDiscovery :: TreacheryCard FearOfDiscovery
fearOfDiscovery = treachery FearOfDiscovery Cards.fearOfDiscovery

-- | When Revealed: The first player must either exhaust the Ring-bearer or reveal an
-- additional encounter card: with the Ring-bearer exhausted already, he reveals it.
-- Shadow: Defending player raises his threat by 3.
instance RunMessage FearOfDiscovery where
  runMessage msg t@(FearOfDiscovery attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      bearers <- select (ringBearer <> ReadyHero)
      chooseOrRunOneM lead do
        questionLabeled "fearOfDiscovery.choose"
        causedBy attrs
        for_ (take 1 bearers) \h -> labeled "fearOfDiscovery.exhaust" $ exhaust h
        labeled "fearOfDiscovery.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid 3
      pure t
    _ -> FearOfDiscovery <$> runMessage msg attrs
