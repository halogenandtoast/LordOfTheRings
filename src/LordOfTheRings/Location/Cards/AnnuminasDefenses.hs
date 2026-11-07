module LordOfTheRings.Location.Cards.AnnuminasDefenses (cityWallAnnuminas, batteringRam) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (damageAnnuminas)

newtype AnnuminasDefenses = AnnuminasDefenses LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

cityWallAnnuminas, batteringRam :: LocationCard AnnuminasDefenses
cityWallAnnuminas = location AnnuminasDefenses Cards.cityWallAnnuminas
batteringRam = location AnnuminasDefenses Cards.batteringRam

instance HasModifiersFor AnnuminasDefenses where
  getModifiersFor (AnnuminasDefenses a) = do
    players <- length <$> getRemainingPlayers
    let ram = toCardCode a == toCardCode Cards.batteringRam
    -- Revised Core rules, Travel Phase: a travel cost must be payable in full.
    -- The discard pile resets only in the quest phase unless an effect says so.
    modifySelf a $ [ImmuneToPlayerCardEffects, TravelRequiresEncounterReveals $ if ram then players else 1]
      <> [ThreatModifier (1 + countTokens Resource a.locationTokens) | ram]

instance HasAbilities AnnuminasDefenses where
  getAbilities (AnnuminasDefenses a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage AnnuminasDefenses where
  runMessage msg l@(AnnuminasDefenses a) = case msg of
    TravelTo lid | lid == a.id -> do
      if toCardCode a == toCardCode Cards.cityWallAnnuminas
        then revealEncounterCard
        else revealEncounterCards . length =<< getRemainingPlayers
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      let resources = countTokens Resource a.locationTokens
      if toCardCode a == toCardCode Cards.batteringRam
        then damageAnnuminas a (resources + 1)
        else when (resources >= 3) do
          discard a a
          damageAnnuminas (CardCodeSource $ toCardCode a) 4
          revealEncounterCard
      pure l
    _ -> AnnuminasDefenses <$> runMessage msg a
