module LordOfTheRings.Location.Cards.HithaeglirFoothills (hithaeglirFoothills) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HithaeglirFoothills = HithaeglirFoothills LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

hithaeglirFoothills :: LocationCard HithaeglirFoothills
hithaeglirFoothills = location HithaeglirFoothills Cards.hithaeglirFoothills

-- | X is the number of resource tokens on this card.
instance HasModifiersFor HithaeglirFoothills where
  getModifiersFor (HithaeglirFoothills a) = do
    let n = countTokens Resource a.locationTokens
    when (n > 0) $ modified a a [ThreatModifier n]

-- | Forced: After a player draws any number of cards, place 1 resource token here.
instance HasAbilities HithaeglirFoothills where
  getAbilities (HithaeglirFoothills a) = [mkAbility a 1 $ forced $ DrewCardsWindow After Nothing]

instance RunMessage HithaeglirFoothills where
  runMessage msg l@(HithaeglirFoothills a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure l
    _ -> HithaeglirFoothills <$> runMessage msg a
