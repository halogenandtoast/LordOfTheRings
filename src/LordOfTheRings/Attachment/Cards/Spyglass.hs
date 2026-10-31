module LordOfTheRings.Attachment.Cards.Spyglass (spyglass, spyglassUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Location.Types (isUncharted)

newtype Spyglass = Spyglass AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon, attached to whom the campaign card's setup says.
spyglass :: AttachmentCard Spyglass
spyglass = attachment Spyglass Cards.spyglass & attachTo AttachToCharacter

spyglassUpgraded :: AttachmentCard Spyglass
spyglassUpgraded = attachment Spyglass Cards.spyglassUpgraded & attachTo AttachToCharacter

-- | Action: Exhaust Spyglass to choose one:
-- • Look at a shadow card. (Upgraded: You may discard it.)
-- • Look at the top and bottom card of your deck. (Upgraded: of any deck.)
-- • Place 1 progress on a location (2 progress instead if it is Uncharted; Upgraded, 4).
instance HasAbilities Spyglass where
  getAbilities (Spyglass a) = [mkAbility a 1 $ ActionAbility (exhaustCost a) | isAttached a.placement]
   where
    isAttached = \case
      AttachedTo _ -> True
      _ -> False

instance RunMessage Spyglass where
  runMessage msg s@(Spyglass a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      let up = isUpgradedSide a
      shadowed <- traverse getEnemy =<< select EnemyWithFacedownShadow
      locations <- traverse getLocation =<< select LocationNotImmuneToPlayerCards
      players <- if up then getRemainingPlayers else pure [pid]
      chooseOneM pid do
        questionLabeled "spyglass.choose"
        causedBy a
        unless (null shadowed) $ labeled "spyglass.shadow" $ chooseOneM pid do
          questionLabeled "spyglass.whichShadow"
          causedBy a
          for_ shadowed \e -> for_ e.shadowCards \c -> targeting (toId e) do
            lookAtShadowCard pid a (toId e) c.id
            when up $ chooseOneM pid do
              questionLabeled "spyglass.discardShadow"
              causedBy a
              showing [EncounterCard c]
              labeled "spyglass.discard" $ discardShadowCard (toId e) c
              labeled "spyglass.keep" $ pure ()
        labeled "spyglass.deck" $ chooseOrRunOneM pid do
          questionLabeled "spyglass.whichDeck"
          causedBy a
          targets players \p -> do
            player <- getPlayer p
            chooseOneM pid do
              questionLabeled "spyglass.topAndBottom"
              causedBy a
              showing $ take 1 player.deck <> take 1 (reverse $ drop 1 player.deck)
              labeled "continue" $ pure ()
        unless (null locations) $ labeled "spyglass.progress" $ chooseOneM pid do
          questionLabeled "spyglass.whichLocation"
          causedBy a
          for_ locations \l -> targeting (toId l) $ placeProgress a (toId l) $ if isUncharted l then (if up then 4 else 2) else 1
      pure s
    _ -> Spyglass <$> removedAsItLeaves msg a
