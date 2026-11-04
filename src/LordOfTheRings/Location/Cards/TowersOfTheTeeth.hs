module LordOfTheRings.Location.Cards.TowersOfTheTeeth (towersOfTheTeeth) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TowersOfTheTeeth = TowersOfTheTeeth LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

towersOfTheTeeth :: LocationCard TowersOfTheTeeth
towersOfTheTeeth = location TowersOfTheTeeth Cards.towersOfTheTeeth

-- | Immune to player card effects. X is the number of characters controlled by the player
-- with the most characters: its threat, printed X. While Towers of the Teeth is the active
-- location, players cannot play allies.
instance HasModifiersFor TowersOfTheTeeth where
  getModifiersFor (TowersOfTheTeeth a) = do
    players <- getRemainingPlayers
    counts <- for players \p -> length <$> selectCharacters (HeroControlledBy $ PlayerWithId p) (AllyControlledBy $ PlayerWithId p)
    let x = fromMaybe 0 (maximumMay counts)
    modifySelf a $ ImmuneToPlayerCardEffects : [ThreatModifier x | x > 0]
    when (a.placement == ActiveLocation) $ modified a GameTarget [CannotPlay (CardWithType AllyType)]

instance RunMessage TowersOfTheTeeth where
  runMessage msg (TowersOfTheTeeth a) = TowersOfTheTeeth <$> runMessage msg a
