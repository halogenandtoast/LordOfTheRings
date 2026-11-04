module LordOfTheRings.Attachment.Cards.DunedainPipe (dunedainPipe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain))

newtype DunedainPipe = DunedainPipe AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain character. Limit 1 per character, on its card def.
dunedainPipe :: AttachmentCard DunedainPipe
dunedainPipe =
  attachment DunedainPipe Cards.dunedainPipe
    & attachTo (AttachToCharacterMatching (HeroWithTrait Dunedain) (AllyWithTrait Dunedain))

-- | Action: Exhaust Dúnedain Pipe and place a card from your hand on the bottom of your
-- deck to draw a card.
instance HasAbilities DunedainPipe where
  getAbilities (DunedainPipe a) =
    [ restricted a 1 (PlayerExists $ PlayerWithId a.controller <> PlayerWithCardsInHand 1)
        $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage DunedainPipe where
  runMessage msg s@(DunedainPipe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      hand <- (.hand) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "dunedainPipe.bottom"
        causedBy a
        targets hand \c -> do
          putFromHandOnBottomOfDeck pid c
          drawCards a pid 1
      pure s
    _ -> DunedainPipe <$> runMessage msg a
