module LordOfTheRings.Attachment.Cards.WarAxe (warAxe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdKeywords, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype WarAxe = WarAxe AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

warAxe :: AttachmentCard WarAxe
warAxe =
  attachment WarAxe Cards.warAxe
    & attachTo (AttachToCharacterMatching (HeroWithSphere Tactics) (AllyWithSphere Tactics))

-- | Limit 1 per character. Restricted. Attached character gets +1 attack for each
-- attachment it has with the restricted keyword, War Axe among them.
instance HasModifiersFor WarAxe where
  getModifiersFor (WarAxe a) = withHost a \host -> do
    restricted <-
      getsGame
        $ filter (\x -> x.placement == AttachedTo host && Restricted `member` cdKeywords (toCardDef x))
        . toList
        . (.entities.attachments)
    modified a host [AttackModifier $ length restricted]
