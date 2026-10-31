module LordOfTheRings.Location.Cards.Eilenach (eilenach) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype Eilenach = Eilenach LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

eilenach :: LocationCard Eilenach
eilenach = location Eilenach Cards.eilenach

-- | Players cannot travel to Eilenach unless there is at least 1 resource in each hero's
-- resource pool.
instance HasModifiersFor Eilenach where
  getModifiersFor (Eilenach a) =
    whenM (selectAny $ not_ HeroWithResources) $ modified a a [CannotBeTraveledTo]

-- | Shadow: If the defending character has no resources, deal it 1 damage. An ally has no
-- resource pool, so none.
instance RunMessage Eilenach where
  runMessage msg l@(Eilenach a) = case msg of
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> do
        broke <- case d of
          HeroTarget hid -> (== 0) . (.resources) <$> getHero hid
          _ -> pure True
        when broke $ dealDamage a d 1
      pure l
    _ -> Eilenach <$> runMessage msg a
