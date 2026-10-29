module LordOfTheRings.Attachment.Cards.TheFallOfGilGalad (theFallOfGilGalad) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdThreatCost, toCardDef)

newtype TheFallOfGilGalad = TheFallOfGilGalad AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Limit 1 per deck, on its card def. Attach to a hero.
theFallOfGilGalad :: AttachmentCard TheFallOfGilGalad
theFallOfGilGalad = attachment TheFallOfGilGalad Cards.theFallOfGilGalad & attachTo (AttachToHero AnyHero)

-- | Response: When attached hero is destroyed, add The Fall of Gil-Galad to the victory
-- display to reduce your threat by X, where X is the attached hero's threat cost.
instance HasAbilities TheFallOfGilGalad where
  getAbilities (TheFallOfGilGalad a) =
    [mkAbility a 1 $ response $ TargetDestroyedWindow When host | AttachedTo host <- [a.placement]]

instance RunMessage TheFallOfGilGalad where
  runMessage msg s@(TheFallOfGilGalad a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      case a.placement of
        AttachedTo (HeroTarget hid) -> do
          x <- fromMaybe 0 . cdThreatCost . toCardDef <$> getHero hid
          addToVictoryDisplay a
          lowerThreatBy a pid x
        _ -> pure ()
      pure s
    _ -> TheFallOfGilGalad <$> runMessage msg a
