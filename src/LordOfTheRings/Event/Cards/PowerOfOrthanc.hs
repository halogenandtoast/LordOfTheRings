module LordOfTheRings.Event.Cards.PowerOfOrthanc (powerOfOrthanc) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype PowerOfOrthanc = PowerOfOrthanc EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 2, on its card def. Action: Each player may choose and discard a Condition
-- attachment from play.
powerOfOrthanc :: EventCard PowerOfOrthanc
powerOfOrthanc = event PowerOfOrthanc Cards.powerOfOrthanc

instance RunMessage PowerOfOrthanc where
  runMessage msg e@(PowerOfOrthanc attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      doStep 0 msg
      pure e
    -- each player in turn, choosing among the conditions those before him left in play
    DoStep n inner@(PlayEvent _ card _) | card.id == toId attrs -> do
      players <- getRemainingPlayers
      for_ (headMay $ drop n players) \p -> do
        conditions <- getConditionAttachments
        chooseOneM p do
          questionLabeled "powerOfOrthanc.discard"
          causedBy attrs
          targets conditions discard
          labeled "pass" (pure ())
        doStep (n + 1) inner
      pure e
    _ -> PowerOfOrthanc <$> runMessage msg attrs
