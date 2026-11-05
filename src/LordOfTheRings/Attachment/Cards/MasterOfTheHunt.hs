module LordOfTheRings.Attachment.Cards.MasterOfTheHunt (masterOfTheHunt) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MasterOfTheHunt = MasterOfTheHunt AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Accolade boon of the Ered Mithrin campaign. Permanent. Setup: Attach to a hero.
masterOfTheHunt :: AttachmentCard MasterOfTheHunt
masterOfTheHunt = attachment MasterOfTheHunt Cards.masterOfTheHunt

-- | Attached hero gets +1 hit point.
instance HasModifiersFor MasterOfTheHunt where
  getModifiersFor (MasterOfTheHunt a) = withHost a \host -> modified a host [HitPointModifier 1]

-- | Response: After attached hero commits to the quest, choose a player to look at the top
-- 3 cards of their deck. They put 1 of those cards on the top of their deck and put the
-- rest on the bottom of their deck in any order.
instance HasAbilities MasterOfTheHunt where
  getAbilities (MasterOfTheHunt a) = [mkAbility a 1 $ response $ CommittedToQuestWindow After host | AttachedTo host <- [a.placement]]

instance RunMessage MasterOfTheHunt where
  runMessage msg s@(MasterOfTheHunt a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      players <- getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "choosePlayer"
        causedBy a
        targets players \p -> do
          top <- take 3 . (.deck) <$> getPlayer p
          chooseOneM p do
            questionLabeled "masterOfTheHunt.top"
            causedBy a
            showing top
            targets top \kept -> do
              -- each in turn goes to the bottom, the last chosen lowest
              let rest = filter ((/= kept.id) . (.id)) top
              chooseOneAtATimeM p do
                questionLabeled "masterOfTheHunt.bottom"
                causedBy a
                showing rest
                targets rest (moveToBottomOfDeck p)
      pure s
    _ -> MasterOfTheHunt <$> runMessage msg a
