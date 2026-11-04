module LordOfTheRings.Attachment.Cards.WizardPipe (wizardPipe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Istari))

newtype WizardPipe = WizardPipe AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to an Istari character. Limit 1 per character, on its card def.
wizardPipe :: AttachmentCard WizardPipe
wizardPipe = attachment WizardPipe Cards.wizardPipe & attachTo (AttachToCharacterMatching (HeroWithTrait Istari) (AllyWithTrait Istari))

-- | Action: Exhaust Wizard Pipe to exchange a card in your hand with the top card of your
-- deck.
instance HasAbilities WizardPipe where
  getAbilities (WizardPipe a) =
    [ restricted a 1 (PlayerExists $ PlayerWithId a.controller <> PlayerWithCardsInHand 1 <> PlayerWithCardsInDeck 1)
        $ ActionAbility (exhaustCost a)
    ]

instance RunMessage WizardPipe where
  runMessage msg s@(WizardPipe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      player <- getPlayer pid
      for_ (headMay player.deck) \top -> chooseOneM pid do
        questionLabeled "wizardPipe.exchange"
        causedBy a
        targets player.hand \c -> swapHandWithDeck pid c top
      pure s
    _ -> WizardPipe <$> runMessage msg a
