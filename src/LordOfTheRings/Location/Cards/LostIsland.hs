module LordOfTheRings.Location.Cards.LostIsland (lostIsland, lostIslandTempleOfTheDeceived) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | The Lost Island side of an Uncharted location, faceup as it enters play: the card under
-- it is its other side.
newtype LostIsland = LostIsland LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

lostIsland :: LocationCard LostIsland
lostIsland = location LostIsland Cards.lostIsland

-- | Temple of the Deceived's own Lost Island side, the same text.
lostIslandTempleOfTheDeceived :: LocationCard LostIsland
lostIslandTempleOfTheDeceived = location LostIsland Cards.lostIslandTempleOfTheDeceived

-- | Cannot have player card attachments.
-- ponytail: no attachment at all, as no encounter card attaches to one
instance HasModifiersFor LostIsland where
  getModifiersFor (LostIsland a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Lost Island becomes the active location, flip it over.
-- Action: Remove 4 progress from Lost Island to look at its facedown side.
instance HasAbilities LostIsland where
  getAbilities (LostIsland a) =
    mkAbility a 1 (forced $ LocationBecameActiveWindow After (LocationWithId a.id))
      : [anyPlayer $ mkAbility a 2 $ ActionAbility Free | a.progress >= 4]

instance RunMessage LostIsland where
  runMessage msg l@(LostIsland a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      removeTokens a a Progress 4
      lookAtFacedownSide pid a.id
      pure l
    _ -> LostIsland <$> runMessage msg a
