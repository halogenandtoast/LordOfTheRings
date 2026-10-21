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

-- ponytail: "Limit 1 per character" is not checked when attaching
warAxe :: AttachmentCard WarAxe
warAxe =
  attachment WarAxe Cards.warAxe
    & attachTo (AttachToCharacterMatching (HeroWithSphere Tactics) (AllyWithSphere Tactics))

-- | Restricted. Attached character gets +1 attack for each attachment it has with the
-- restricted keyword (War Axe among them, though its card def lacks the keyword).
instance HasModifiersFor WarAxe where
  getModifiersFor (WarAxe a) = withHost a \host -> do
    others <-
      getsGame
        $ filter (\x -> x.placement == AttachedTo host && toId x /= a.id && Restricted `member` cdKeywords (toCardDef x))
        . toList
        . (.entities.attachments)
    modified a host [AttackModifier $ 1 + length others]
