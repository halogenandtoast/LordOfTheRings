module LordOfTheRings.Location.Cards.EastGate (eastGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype EastGate = EastGate LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastGate :: LocationCard EastGate
eastGate = location EastGate Cards.eastGate

-- | Immune to card effects.
-- Players cannot optionally engage enemies and no engagement checks are made.
instance HasModifiersFor EastGate where
  getModifiersFor (EastGate a) = do
    modified a a [ImmuneToCardEffects]
    modified a GameTarget [CannotBeOptionallyEngagedBy AnyPlayer, NoEngagementChecks]

-- | Forced: After East-gate leaves play as an explored location, add First Hall to the
-- staging area.
instance HasAbilities EastGate where
  getAbilities (EastGate a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage EastGate where
  runMessage msg l@(EastGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      addSetAsideToStaging (cardIs Cards.firstHall)
      pure l
    _ -> EastGate <$> runMessage msg a
