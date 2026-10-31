module LordOfTheRings.Treachery.Cards.LostAndHelpless (lostAndHelpless) where

import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (lostTriggered)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LostAndHelpless = LostAndHelpless TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostAndHelpless :: TreacheryCard LostAndHelpless
lostAndHelpless = treachery LostAndHelpless Cards.lostAndHelpless

-- | Surge.
-- When Revealed: Attach to a hero. (Counts as a Condition attachment with the text: "Lost:
-- Discard attached hero.")
-- Shadow: If you have no cards in your hand, discard defending character from play.
instance RunMessage LostAndHelpless where
  runMessage msg t@(LostAndHelpless attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      heroes <- select AnyHero
      chooseOrRunOneM lead do
        questionLabeled "lostAndHelpless.attach"
        causedBy attrs
        targets heroes \h -> attachTreachery attrs.id h
      pure t
    _ | lostTriggered attrs msg -> do
      withHost attrs discard
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      hand <- discardableHand <$> getPlayer pid
      when (null hand) $ traverse_ discard defenders
      pure t
    _ -> LostAndHelpless <$> runMessage msg attrs
