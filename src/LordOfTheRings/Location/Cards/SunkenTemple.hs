module LordOfTheRings.Location.Cards.SunkenTemple (sunkenTemple) where

import LordOfTheRings.Ally.Types (allyTokens)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (mayFlipToGrottoSide)

-- | Underwater side of Water-logged Halls.
newtype SunkenTemple = SunkenTemple LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

sunkenTemple :: LocationCard SunkenTemple
sunkenTemple = location SunkenTemple Cards.sunkenTemple

-- | While Sunken Temple is the active location, treat each attachment as if its printed
-- text box is blank (except for Traits).
instance HasModifiersFor SunkenTemple where
  getModifiersFor (SunkenTemple a) = when (a.placement == ActiveLocation) $ modified a GameTarget [BlankAttachments]

-- | Forced: If Sunken Temple is the active location at the end of the quest phase, discard
-- 1 resource from each objective-ally and each hero's resource pool. Then, you may flip
-- Sunken Temple to its Grotto side.
instance HasAbilities SunkenTemple where
  getAbilities (SunkenTemple a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]

instance RunMessage SunkenTemple where
  runMessage msg l@(SunkenTemple a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      heroes <- select HeroWithResources
      for_ heroes \h -> removeTokens a h Resource 1
      allies <- select ObjectiveAlly
      for_ allies \o -> do
        resources <- countTokens Resource . allyTokens . toAttrs <$> getAlly o
        when (resources > 0) $ removeTokens a o Resource 1
      mayFlipToGrottoSide a
      pure l
    _ -> SunkenTemple <$> runMessage msg a
