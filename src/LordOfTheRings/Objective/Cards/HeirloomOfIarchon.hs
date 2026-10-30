module LordOfTheRings.Objective.Cards.HeirloomOfIarchon (heirloomOfIarchon) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (clueAbilities, runClue)

newtype HeirloomOfIarchon = HeirloomOfIarchon ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

heirloomOfIarchon :: ObjectiveCard HeirloomOfIarchon
heirloomOfIarchon = objective HeirloomOfIarchon Cards.heirloomOfIarchon

-- | Action: Exhaust Amarthiúl or a hero to claim Heirloom of Iârchon. Then, attach Heirloom
-- of Iârchon to that character.
instance HasAbilities HeirloomOfIarchon where
  getAbilities (HeirloomOfIarchon a) = clueAbilities a

-- | Attached character gets +1 willpower.
instance HasModifiersFor HeirloomOfIarchon where
  getModifiersFor (HeirloomOfIarchon a) = withHost a \host -> modified a host [WillpowerModifier 1]

instance RunMessage HeirloomOfIarchon where
  runMessage msg (HeirloomOfIarchon attrs) = HeirloomOfIarchon <$> runClue msg attrs
