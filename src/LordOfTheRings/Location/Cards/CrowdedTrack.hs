module LordOfTheRings.Location.Cards.CrowdedTrack (crowdedTrack) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype CrowdedTrack = CrowdedTrack LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

crowdedTrack :: LocationCard CrowdedTrack
crowdedTrack = location CrowdedTrack Cards.crowdedTrack

-- | Crowded Track gets +1 Threat for each character controlled by the first player.
instance HasModifiersFor CrowdedTrack where
  getModifiersFor (CrowdedTrack a) = do
    n <- length <$> (charactersControlledBy =<< getLead)
    when (n > 0) $ modifySelf a [ThreatModifier n]

-- | Forced: After Crowded Track becomes the active location, each player searches the
-- encounter deck and discard pile for an enemy and adds it to the staging area. Shuffle the
-- encounter deck.
instance HasAbilities CrowdedTrack where
  getAbilities (CrowdedTrack a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage CrowdedTrack where
  runMessage msg l@(CrowdedTrack a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p a (CardWithType EnemyType)
      pure l
    _ -> CrowdedTrack <$> runMessage msg a
