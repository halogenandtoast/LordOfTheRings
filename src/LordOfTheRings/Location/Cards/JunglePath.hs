module LordOfTheRings.Location.Cards.JunglePath (junglePath) where

import LordOfTheRings.Helpers.Character (getAttack)
import LordOfTheRings.Helpers.IslandMap (locationToThe)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Grid (Direction (Above, Below))
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under a Lost Island side.
newtype JunglePath = JunglePath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

junglePath :: LocationCard JunglePath
junglePath = location JunglePath Cards.junglePath

-- | While Jungle Path is the active location, the locations immediately above and below
-- it are not considered to be in the staging area.
instance HasModifiersFor JunglePath where
  getModifiersFor (JunglePath a) = when (a.placement == ActiveLocation) do
    for_ [Above, Below] \d -> locationToThe d a >>= traverse_ \l -> modified a l [NotInStagingArea]

-- | Forced: When Jungle Path is flipped over while active, each player must exhaust a
-- character with at least 2 attack.
instance HasAbilities JunglePath where
  getAbilities (JunglePath a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage JunglePath where
  runMessage msg l@(JunglePath a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        untapped <- selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
        strong <- filterM (fmap (>= 2) . getAttack) untapped
        chooseOneM p do
          questionLabeled "junglePath.exhaust"
          causedBy a
          targets strong exhaust
      pure l
    _ -> JunglePath <$> runMessage msg a
