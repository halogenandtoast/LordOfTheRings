module LordOfTheRings.Location.Cards.WoodmansGlade (woodmansGlade) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype WoodmansGlade = WoodmansGlade LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodmansGlade :: LocationCard WoodmansGlade
woodmansGlade = location WoodmansGlade Cards.woodmansGlade

-- | Travel: The player guarding Gollum must exhaust a hero he controls to travel to
-- Woodman's Glade.
instance HasModifiersFor WoodmansGlade where
  getModifiersFor (WoodmansGlade a) = do
    guard' <- getGuard
    able <- maybe (pure False) (\g -> selectAny (HeroControlledBy (PlayerWithId g) <> ReadyHero)) guard'
    unless able $ modified a a [CannotBeTraveledTo]

-- | Response: After exploring Woodman's Glade, reduce the threat of each player not
-- guarding Gollum by 2.
instance HasAbilities WoodmansGlade where
  getAbilities (WoodmansGlade a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

instance RunMessage WoodmansGlade where
  runMessage msg l@(WoodmansGlade a) = case msg of
    TravelTo lid | lid == a.id -> do
      getGuard >>= traverse_ \g -> do
        heroes <- select (HeroControlledBy (PlayerWithId g) <> ReadyHero)
        chooseOrRunOneM g do
          questionLabeled "woodmansGlade.exhaust"
          causedBy a
          targets heroes exhaust
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      guard' <- getGuard
      eachPlayer \p -> unless (Just p == guard') $ lowerThreat p 2
      pure l
    _ -> WoodmansGlade <$> runMessage msg a
