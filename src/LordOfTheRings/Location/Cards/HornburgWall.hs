module LordOfTheRings.Location.Cards.HornburgWall (hornburgWall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Keyword (Keyword (Toughness))

newtype HornburgWall = HornburgWall LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

hornburgWall :: LocationCard HornburgWall
hornburgWall = location HornburgWall Cards.hornburgWall

instance HasModifiersFor HornburgWall where
  getModifiersFor (HornburgWall a) = do
    enemies <- select AnyEnemy
    for_ enemies \eid -> modified a eid [GainKeyword $ Toughness 1]

instance HasAbilities HornburgWall where
  getAbilities (HornburgWall a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage HornburgWall where
  runMessage msg l@(HornburgWall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      allies <- filterM (fmap not . immuneToSource (toSource a)) =<< select AnyAlly
      traverse_ exhaust allies
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardNonObjectiveAttachment a pid
      pure l
    _ -> HornburgWall <$> runMessage msg a
