module LordOfTheRings.Treachery.Cards.CatacombInspection (catacombInspection) where

import LordOfTheRings.Trait (Trait (DolGuldur))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CatacombInspection = CatacombInspection TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

catacombInspection :: TreacheryCard CatacombInspection
catacombInspection = treachery CatacombInspection Cards.catacombInspection

-- | Surge.
-- When Revealed: Add 1 resource token to each Dol Guldur location in play.
-- Shadow: If there is a Dol Guldur location in the staging area, move 1 resource token from
-- each of your heroes' resource pools to that location. (Resolve this effect for each Dol
-- Guldur location.)
instance RunMessage CatacombInspection where
  runMessage msg t@(CatacombInspection attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- select (LocationWithTrait DolGuldur)
      for_ locations \l -> placeResources attrs l 1
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      locations <- select (LocationWithTrait DolGuldur <> LocationInStagingArea)
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      -- each hero gives as many as it has, up to one for each location
      for_ heroes \h ->
        for_ (take h.resources locations) \l -> do
          removeTokens attrs h Resource 1
          placeResources attrs l 1
      pure t
    _ -> CatacombInspection <$> runMessage msg attrs
