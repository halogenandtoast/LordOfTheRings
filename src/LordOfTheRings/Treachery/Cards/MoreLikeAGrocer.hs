module LordOfTheRings.Treachery.Cards.MoreLikeAGrocer (moreLikeAGrocer) where

import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MoreLikeAGrocer = MoreLikeAGrocer TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

moreLikeAGrocer :: TreacheryCard MoreLikeAGrocer
moreLikeAGrocer = treachery MoreLikeAGrocer Cards.moreLikeAGrocer

-- | When Revealed: Discard all Baggins resources: those in the pools of Baggins sphere
-- heroes.
-- Shadow: Resolve this card's "When Revealed" effect.
instance RunMessage MoreLikeAGrocer where
  runMessage msg t@(MoreLikeAGrocer attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardBagginsResources
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      discardBagginsResources
      pure t
    _ -> MoreLikeAGrocer <$> runMessage msg attrs
   where
    discardBagginsResources = do
      heroes <- traverse getHero =<< select (HeroWithSphere Baggins <> HeroWithResources)
      for_ heroes \h -> removeTokens attrs h Resource h.resources
