module LordOfTheRings.Location.Cards.RiverLangflood (riverLangflood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Troll))

newtype RiverLangflood = RiverLangflood LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

riverLangflood :: LocationCard RiverLangflood
riverLangflood = location RiverLangflood Cards.riverLangflood

-- | While it is in the staging area, River Langflood gets +1 threat for each Troll enemy in
-- play.
instance HasModifiersFor RiverLangflood where
  getModifiersFor (RiverLangflood a) = when (a.placement == StagingArea) do
    trolls <- selectCount (EnemyWithTrait Troll)
    when (trolls > 0) $ modified a a [ThreatModifier trolls]
