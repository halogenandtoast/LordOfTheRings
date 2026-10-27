module LordOfTheRings.Attachment.Cards.KeepingCount (keepingCount) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Game.Base (gameEntities)

newtype KeepingCount = KeepingCount AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, as its card def says.
keepingCount :: AttachmentCard KeepingCount
keepingCount = attachment KeepingCount Cards.keepingCount & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +1 attack for each resource token on another copy of Keeping
-- Count that is above the current number of resource tokens on this card.
instance HasModifiersFor KeepingCount where
  getModifiersFor (KeepingCount a) = withHost a \host -> do
    others <-
      getsGame
        (filter (\x -> toCardCode x == toCardCode a && toId x /= a.id) . toList . entitiesAttachments . gameEntities)
    let mine = countTokens Resource a.attachmentTokens
        above = sum [max 0 (countTokens Resource (toAttrs x).attachmentTokens - mine) | x <- others]
    when (above > 0) $ modified a host [AttackModifier above]

-- | Forced: After attached hero attacks and destroys an enemy, place 1 resource token on
-- this card.
instance HasAbilities KeepingCount where
  getAbilities (KeepingCount a) =
    [mkAbility a 1 $ forced $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage KeepingCount where
  runMessage msg s@(KeepingCount a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure s
    _ -> KeepingCount <$> runMessage msg a
