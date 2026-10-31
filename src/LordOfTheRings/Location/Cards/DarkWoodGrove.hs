module LordOfTheRings.Location.Cards.DarkWoodGrove (darkWoodGrove) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel.Helpers (athelasInPlay)

newtype DarkWoodGrove = DarkWoodGrove LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkWoodGrove :: LocationCard DarkWoodGrove
darkWoodGrove = location DarkWoodGrove Cards.darkWoodGrove

-- | Dark-wood Grove gets +1 threat and +1 quest point for each Athelas card in play.
instance HasModifiersFor DarkWoodGrove where
  getModifiersFor (DarkWoodGrove a) = do
    n <- length <$> athelasInPlay
    when (n > 0) $ modifySelf a [ThreatModifier n, QuestPointModifier n]

-- | Shadow: If defending player controls at least 1 Athelas card, attacking enemy makes an
-- additional attack after this one.
instance RunMessage DarkWoodGrove where
  runMessage msg l@(DarkWoodGrove a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
      athelas <- traverse getObjective =<< athelasInPlay
      let controls = any (\o -> o.placement `elem` map AttachedTo heroes) athelas
      when controls $ additionalAttack pid eid
      pure l
    _ -> DarkWoodGrove <$> runMessage msg a
