module LordOfTheRings.Location.Cards.DarkAbyss (darkAbyss) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (mayFlipToGrottoSide)

-- | Underwater side of Undersea Grotto.
newtype DarkAbyss = DarkAbyss LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

darkAbyss :: LocationCard DarkAbyss
darkAbyss = location DarkAbyss Cards.darkAbyss

-- | While Dark Abyss is the active location, characters cannot attack.
instance HasModifiersFor DarkAbyss where
  getModifiersFor (DarkAbyss a) = when (a.placement == ActiveLocation) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotAttack]

-- | Forced: If Dark Abyss is the active location at the end of the quest phase, deal 1
-- damage to each character in play. Then, you may flip Dark Abyss to its Grotto side.
instance HasAbilities DarkAbyss where
  getAbilities (DarkAbyss a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]

instance RunMessage DarkAbyss where
  runMessage msg l@(DarkAbyss a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> dealDamage a c 1
      mayFlipToGrottoSide a
      pure l
    _ -> DarkAbyss <$> runMessage msg a
