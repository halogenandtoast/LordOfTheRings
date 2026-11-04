module LordOfTheRings.Location.Cards.ReekingMoat (reekingMoat) where

import LordOfTheRings.Card (Card (EncounterCard), toCard)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Nazgul))

newtype ReekingMoat = ReekingMoat LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

reekingMoat :: LocationCard ReekingMoat
reekingMoat = location ReekingMoat Cards.reekingMoat

-- | Forced: After Reeking Moat becomes the active location, choose a non-unique, non-Nazgûl
-- enemy in the staging area and place it facedown under Reeking Moat.
instance HasAbilities ReekingMoat where
  getAbilities (ReekingMoat a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

{- | The enemy leaves play for underneath it, as an enemy placed under a location does.
Forced: When Reeking Moat leaves play, put each enemy under it into play engaged with the
first player. Read as it is discarded (explored) or removed from the game, while it still
holds them.
-}
instance RunMessage ReekingMoat where
  runMessage msg l@(ReekingMoat a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy <> not_ (EnemyWithTrait Nazgul))
      chooseOrRunOneM lead do
        questionLabeled "reekingMoat.choose"
        causedBy a
        targets enemies \e -> do
          card <- toCard <$> getEnemy e
          placeUnderneath a [card]
      pure l
    Discard (isTarget a -> True) -> release
    RemoveFromGame (isTarget a -> True) -> release
    _ -> ReekingMoat <$> runMessage msg a
   where
    release = do
      lead <- getLead
      for_ [c | EncounterCard c <- a.underneath] (putEnemyIntoPlayEngaged lead)
      ReekingMoat <$> runMessage msg (a {locationUnderneath = []})
