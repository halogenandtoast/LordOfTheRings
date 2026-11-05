module LordOfTheRings.Location.Cards.CobwebbedTunnel (cobwebbedTunnel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Underground))

newtype CobwebbedTunnel = CobwebbedTunnel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cobwebbedTunnel :: LocationCard CobwebbedTunnel
cobwebbedTunnel = location CobwebbedTunnel Cards.cobwebbedTunnel

instance HasModifiersFor CobwebbedTunnel where
  getModifiersFor (CobwebbedTunnel a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    when (a.placement == StagingArea) do
      locations <- select (LocationInStagingArea <> LocationWithTrait Underground)
      for_ locations \lid -> modified a lid [ThreatModifier 1, QuestPointModifier 5]

instance RunMessage CobwebbedTunnel where
  runMessage msg (CobwebbedTunnel a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCardAs =<< getLead
      CobwebbedTunnel <$> runMessage msg a
    _ -> CobwebbedTunnel <$> runMessage msg a
