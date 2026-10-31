module LordOfTheRings.Location.Cards.DocksideStreet (docksideStreet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Scroll))

newtype DocksideStreet = DocksideStreet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

docksideStreet :: LocationCard DocksideStreet
docksideStreet = location DocksideStreet Cards.docksideStreet

scrollBearer :: HeroMatcher
scrollBearer = HeroWithAttached Scroll <> ReadyHero

-- | While Dockside Street is in the staging area, enemies in the staging area cannot be
-- damaged or optionally engaged.
-- Travel: Exhaust the hero with Alcaron's Scroll attached to travel here.
instance HasModifiersFor DocksideStreet where
  getModifiersFor (DocksideStreet a) = do
    when (a.placement == StagingArea) do
      enemies <- select EnemyInStagingArea
      for_ enemies \e -> modified a e [CannotTakeDamage, CannotBeOptionallyEngagedBy AnyPlayer]
    unable <- selectNone scrollBearer
    when unable $ modified a a [CannotBeTraveledTo]

instance RunMessage DocksideStreet where
  runMessage msg l@(DocksideStreet a) = case msg of
    TravelTo lid | lid == a.id -> do
      traverse_ exhaust =<< select scrollBearer
      pure l
    _ -> DocksideStreet <$> runMessage msg a
