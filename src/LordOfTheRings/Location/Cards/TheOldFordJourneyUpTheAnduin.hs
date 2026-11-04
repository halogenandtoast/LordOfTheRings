module LordOfTheRings.Location.Cards.TheOldFordJourneyUpTheAnduin (theOldFordJourneyUpTheAnduin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheOldFordJourneyUpTheAnduin = TheOldFordJourneyUpTheAnduin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldFordJourneyUpTheAnduin :: LocationCard TheOldFordJourneyUpTheAnduin
theOldFordJourneyUpTheAnduin = location TheOldFordJourneyUpTheAnduin Cards.theOldFordJourneyUpTheAnduin

-- | Immune to player card effects. The Old Ford gets +5 quest points for each enemy in play.
instance HasModifiersFor TheOldFordJourneyUpTheAnduin where
  getModifiersFor (TheOldFordJourneyUpTheAnduin a) = do
    enemies <- selectCount AnyEnemy
    modified a a $ ImmuneToPlayerCardEffects : [QuestPointModifier (5 * enemies) | enemies > 0]

-- | Forced: After The Old Ford becomes the active location, each enemy in the staging area
-- makes an immediate attack against the first player.
instance HasAbilities TheOldFordJourneyUpTheAnduin where
  getAbilities (TheOldFordJourneyUpTheAnduin a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage TheOldFordJourneyUpTheAnduin where
  runMessage msg l@(TheOldFordJourneyUpTheAnduin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      enemies <- select EnemyInStagingArea
      for_ enemies $ immediateAttack lead
      pure l
    _ -> TheOldFordJourneyUpTheAnduin <$> runMessage msg a
