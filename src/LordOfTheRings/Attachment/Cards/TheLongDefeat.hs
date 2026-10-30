module LordOfTheRings.Attachment.Cards.TheLongDefeat (theLongDefeat) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype TheLongDefeat = TheLongDefeat AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a quest card in play. Limit 1 per quest, on its card def.
theLongDefeat :: AttachmentCard TheLongDefeat
theLongDefeat = attachment TheLongDefeat Cards.theLongDefeat & attachTo AttachToQuest

-- | Response: After attached quest card is defeated, each player either draws 2 cards or
-- heals up to 5 damage from among characters he controls.
instance HasAbilities TheLongDefeat where
  getAbilities (TheLongDefeat a) =
    [mkAbility a 1 $ response $ QuestDefeatedWindow After host | AttachedTo host <- [a.placement]]

instance RunMessage TheLongDefeat where
  runMessage msg s@(TheLongDefeat a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeled "theLongDefeat.chooseOne"
        causedBy a
        labeled "theLongDefeat.draw" $ drawCards a p 2
        labeled "theLongDefeat.heal" $ doStep 5 (UseCardAbility p (toSource a) 1 [])
      pure s
    -- the player heals the damage a point at a time, as many as are left
    DoStep n (UseCardAbility p (isSource a -> True) 1 _) | n > 0 -> do
      let you = PlayerWithId p
      damaged <- damagedCharacters (HeroControlledBy you) (AllyControlledBy you)
      chooseOneM p do
        questionLabeledWith "theLongDefeat.healOne" [intVar "count" n]
        causedBy a
        targets damaged \c -> do
          healDamage a c 1
          doStep (n - 1) (UseCardAbility p (toSource a) 1 [])
        unless (null damaged) $ labeled "done" (pure ())
      pure s
    _ -> TheLongDefeat <$> runMessage msg a
