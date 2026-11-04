module LordOfTheRings.Location.Cards.NarrowAlleyway (narrowAlleyway) where

import LordOfTheRings.Card (Card (EncounterCard), toCard)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype NarrowAlleyway = NarrowAlleyway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowAlleyway :: LocationCard NarrowAlleyway
narrowAlleyway = location NarrowAlleyway Cards.narrowAlleyway

-- | Response: After the players travel here, choose a non-unique enemy engaged with a
-- player and place it faceup under this location. While an enemy is under this location,
-- that enemy is considered to be out of play.
instance HasAbilities NarrowAlleyway where
  getAbilities (NarrowAlleyway a) = [restricted a 1 (exists hideable) $ response $ TraveledWindow After a.id]

hideable :: EnemyMatcher
hideable = EnemyEngagedWith AnyPlayer <> not_ UniqueEnemy

{- | The enemy leaves play for underneath it, as an enemy placed under a location does: its
tokens, attachments and shadow cards gone.
Forced: After Narrow Alleyway leaves play, put each enemy under it into play, engaged with
the first player. Read as it is discarded (explored) or removed from the game, while it
still holds them.
-}
instance RunMessage NarrowAlleyway where
  runMessage msg l@(NarrowAlleyway a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select hideable
      chooseOrRunOneM pid do
        questionLabeled "narrowAlleyway.choose"
        causedBy a
        targets enemies \e -> do
          card <- toCard <$> getEnemy e
          placeUnderneath a [card]
      pure l
    Discard (isTarget a -> True) -> release
    RemoveFromGame (isTarget a -> True) -> release
    _ -> NarrowAlleyway <$> runMessage msg a
   where
    release = do
      lead <- getLead
      for_ [c | EncounterCard c <- a.underneath] (putEnemyIntoPlayEngaged lead)
      NarrowAlleyway <$> runMessage msg (a {locationUnderneath = []})
