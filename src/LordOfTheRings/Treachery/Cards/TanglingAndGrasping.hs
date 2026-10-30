module LordOfTheRings.Treachery.Cards.TanglingAndGrasping (tanglingAndGrasping) where

import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TanglingAndGrasping = TanglingAndGrasping TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tanglingAndGrasping :: TreacheryCard TanglingAndGrasping
tanglingAndGrasping = treachery TanglingAndGrasping Cards.tanglingAndGrasping

-- | When Revealed: Each player must remove a character from the quest. If the active
-- location is Underwater, deal 2 damage to each character removed from the quest by this
-- effect.
-- Shadow: Discard an attachment you control.
instance RunMessage TanglingAndGrasping where
  runMessage msg t@(TanglingAndGrasping attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      underwater <- underwaterActive
      eachPlayer \p -> do
        let you = PlayerWithId p
        committed <- selectCharacters (HeroControlledBy you <> CommittedHero) (AllyControlledBy you <> CommittedAlly)
        chooseOrRunOneM p do
          questionLabeled "tanglingAndGrasping.remove"
          causedBy attrs
          targets committed \c -> do
            removeFromQuest c
            when underwater $ dealDamage attrs c 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure t
    _ -> TanglingAndGrasping <$> runMessage msg attrs
