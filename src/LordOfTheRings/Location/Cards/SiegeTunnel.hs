module LordOfTheRings.Location.Cards.SiegeTunnel (siegeTunnel) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Epic.Types (EpicEffect (EpicDiscardUntilEnemy))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (eachAnnuminasTeam)

newtype SiegeTunnel = SiegeTunnel LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

siegeTunnel :: LocationCard SiegeTunnel
siegeTunnel = location SiegeTunnel Cards.siegeTunnel

instance HasModifiersFor SiegeTunnel where
  getModifiersFor (SiegeTunnel a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ImmuneToPlayerCardEffects, TravelRequiresEncounterReveals players]

instance HasAbilities SiegeTunnel where
  getAbilities (SiegeTunnel a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage SiegeTunnel where
  runMessage msg l@(SiegeTunnel a) = case msg of
    TravelTo lid | lid == a.id -> l <$ (revealEncounterCards . length =<< getRemainingPlayers)
    UseCardAbility _ (isSource a -> True) 1 _ ->
      l <$ eachAnnuminasTeam (toCardCode a) EpicDiscardUntilEnemy
    _ -> SiegeTunnel <$> runMessage msg a
