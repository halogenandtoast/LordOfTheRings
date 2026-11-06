module LordOfTheRings.Attachment.Cards.KeenLongbow (keenLongbow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Sphere (Sphere (Lore))

newtype KeenLongbow = KeenLongbow AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Lore hero. Limit 1 per hero. Restricted, on its card def.
keenLongbow :: AttachmentCard KeenLongbow
keenLongbow = attachment KeenLongbow Cards.keenLongbow & attachTo (AttachToHero $ HeroWithSphere Lore)

-- | Attached hero gains ranged.
instance HasModifiersFor KeenLongbow where
  getModifiersFor (KeenLongbow a) = withHost a \host -> modified a host [GainKeyword Ranged]

-- | Response: After attached hero is declared as an attacker against an enemy, discard up
-- to 3 cards from your hand to deal 1 damage to that enemy for each card discarded this
-- way.
--
-- The cards are discarded one at a time, step n having discarded n; the damage is dealt
-- once the discarding is done.
instance HasAbilities KeenLongbow where
  getAbilities (KeenLongbow a) =
    [ restricted a 1 (CardInHandToDiscard a.controller AnyCard) $ response $ AttackerDeclaredWindow After (Just host)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage KeenLongbow where
  runMessage msg s@(KeenLongbow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      doStep 0 msg
      pure s
    DoStep n inner@(UseCardAbility pid (isSource a -> True) 1 windows) ->
      s <$ for_ (take 1 [eid | AttachedTo host <- [a.placement], Window _ (DeclaredAsAttacker t eid) <- windows, t == host]) \eid -> do
        hand <- discardableHand <$> getPlayer pid
        if n >= 3 || null hand
          then when (n > 0) $ dealDamage a eid n
          else chooseOneM pid do
            questionLabeled "keenLongbow.discard"
            causedBy a
            targets hand \c -> do
              discardFromHand pid c
              doStep (n + 1) inner
            labeled "doneDiscarding" $ when (n > 0) $ dealDamage a eid n
    _ -> KeenLongbow <$> runMessage msg a
