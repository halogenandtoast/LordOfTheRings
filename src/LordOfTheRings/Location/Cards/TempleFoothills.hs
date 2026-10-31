module LordOfTheRings.Location.Cards.TempleFoothills (templeFoothills) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under an Edge of the Temple side.
newtype TempleFoothills = TempleFoothills LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

templeFoothills :: LocationCard TempleFoothills
templeFoothills = location TempleFoothills Cards.templeFoothills

-- | While Temple Foothills is the active location, the players, as a group, cannot play
-- more than 1 card each round: once any of them has played one this round, none plays
-- another.
instance HasModifiersFor TempleFoothills where
  getModifiersFor (TempleFoothills a) = when (a.placement == ActiveLocation) do
    played <- getsGame \g -> or [PlayedACardThisRound `elem` ms | Effect _ (PlayerTarget _) ms _ <- gameEffects g]
    when played do
      players <- getRemainingPlayers
      for_ players \p -> modified a (PlayerTarget p) [CannotPlay AnyCard]

instance RunMessage TempleFoothills where
  runMessage msg (TempleFoothills a) = TempleFoothills <$> runMessage msg a
