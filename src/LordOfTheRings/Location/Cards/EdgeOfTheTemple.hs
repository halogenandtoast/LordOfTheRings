module LordOfTheRings.Location.Cards.EdgeOfTheTemple (edgeOfTheTemple) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

-- | The Edge of the Temple side of Temple of the Deceived's nightmare Uncharted locations,
-- faceup as they enter play: the card under it is its other side.
newtype EdgeOfTheTemple = EdgeOfTheTemple LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

edgeOfTheTemple :: LocationCard EdgeOfTheTemple
edgeOfTheTemple = location EdgeOfTheTemple Cards.edgeOfTheTemple

-- | Cannot have player card attachments.
-- ponytail: no attachment at all, as Lost Island
instance HasModifiersFor EdgeOfTheTemple where
  getModifiersFor (EdgeOfTheTemple a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Edge of the Temple becomes the active location, flip it over.
-- Action: Remove 8 progress from Edge of the Temple to look at its facedown side.
instance HasAbilities EdgeOfTheTemple where
  getAbilities (EdgeOfTheTemple a) =
    mkAbility a 1 (forced $ LocationBecameActiveWindow After (LocationWithId a.id))
      : [anyPlayer $ mkAbility a 2 $ ActionAbility Free | a.progress >= 8]

instance RunMessage EdgeOfTheTemple where
  runMessage msg l@(EdgeOfTheTemple a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      removeTokens a a Progress 8
      lookAtFacedownSide pid a.id
      pure l
    _ -> EdgeOfTheTemple <$> runMessage msg a
