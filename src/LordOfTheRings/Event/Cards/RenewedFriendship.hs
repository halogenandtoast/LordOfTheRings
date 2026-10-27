module LordOfTheRings.Event.Cards.RenewedFriendship (renewedFriendship) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype RenewedFriendship = RenewedFriendship EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After another player plays an attachment on a hero you control, you may
-- (choose 1): ready 1 of that player's heroes, have that player draw 1 card, or lower
-- that player's threat by 2.
renewedFriendship :: EventCard RenewedFriendship
renewedFriendship =
  eventWith RenewedFriendship Cards.renewedFriendship \a -> a & windowL ?~ AttachmentPlayedOnHeroWindow After a.owner

instance RunMessage RenewedFriendship where
  runMessage msg e@(RenewedFriendship attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [p | Window _ (PlayedCardFromHand p _) <- windows]) \p -> do
        exhausted <- select (HeroControlledBy (PlayerWithId p) <> ExhaustedHero)
        chooseOneM pid do
          questionLabeled "renewedFriendship.choose"
          causedBy attrs
          unless (null exhausted) $ labeled "renewedFriendship.ready" $ chooseOneM pid do
            questionLabeled "renewedFriendship.chooseHero"
            causedBy attrs
            targets exhausted ready
          labeled "renewedFriendship.draw" $ drawCards p 1
          labeled "renewedFriendship.lowerThreat" $ lowerThreat p 2
      pure e
    _ -> RenewedFriendship <$> runMessage msg attrs
