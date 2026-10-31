module LordOfTheRings.Location.Cards.MethedrasPeak (methedrasPeak) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (searches)

newtype MethedrasPeak = MethedrasPeak LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

methedrasPeak :: LocationCard MethedrasPeak
methedrasPeak = location MethedrasPeak Cards.methedrasPeak

-- | X is the number of characters controlled by the first player.
instance HasModifiersFor MethedrasPeak where
  getModifiersFor (MethedrasPeak a) = do
    lead <- getLead
    x <- length <$> selectCharacters (HeroControlledBy $ PlayerWithId lead) (AllyControlledBy $ PlayerWithId lead)
    modified a a [ThreatModifier x]

-- | Forced: After Methedras Peak leaves play as an explored location, each player Searches 3.
instance HasAbilities MethedrasPeak where
  getAbilities (MethedrasPeak a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage MethedrasPeak where
  runMessage msg l@(MethedrasPeak a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> searches p 3
      pure l
    _ -> MethedrasPeak <$> runMessage msg a
