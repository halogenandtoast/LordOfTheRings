module LordOfTheRings.Location.Cards.EnemyCamp (enemyCamp) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Epic.Types (EpicEffect (EpicDiscardCaptive, EpicRescueCaptives))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (eachAnnuminasTeam)

newtype EnemyCamp = EnemyCamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

enemyCamp :: LocationCard EnemyCamp
enemyCamp = location EnemyCamp Cards.enemyCamp

instance HasModifiersFor EnemyCamp where
  getModifiersFor (EnemyCamp a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ImmuneToPlayerCardEffects, TravelRequiresEncounterReveals players]

instance HasAbilities EnemyCamp where
  getAbilities (EnemyCamp a) = [mkAbility a 1 $ forced RoundEndsWindow]
  getVictoryAbilities (EnemyCamp a) = [mkAbility a 2 $ forced RoundEndsWindow]

instance RunMessage EnemyCamp where
  runMessage msg l@(EnemyCamp a) = case msg of
    TravelTo lid | lid == a.id -> l <$ (revealEncounterCards . length =<< getRemainingPlayers)
    UseCardAbility _ (isSource a -> True) 1 _ ->
      l <$ eachAnnuminasTeam (toCardCode a) EpicDiscardCaptive
    UseCardAbility _ (isSource a -> True) 2 _ ->
      l <$ eachAnnuminasTeam (toCardCode a) EpicRescueCaptives
    _ -> EnemyCamp <$> runMessage msg a
