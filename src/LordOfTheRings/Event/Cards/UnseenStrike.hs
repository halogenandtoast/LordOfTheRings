module LordOfTheRings.Event.Cards.UnseenStrike (unseenStrike) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype UnseenStrike = UnseenStrike EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a character you control. Until the end of the phase, that character
-- gets +3 attack while attacking an enemy with a higher engagement cost than your threat.
unseenStrike :: EventCard UnseenStrike
unseenStrike = event UnseenStrike Cards.unseenStrike

instance RunMessage UnseenStrike where
  runMessage msg e@(UnseenStrike attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      characters <- charactersControlledBy pid
      chooseOneM pid do
        questionLabeled "unseenStrike.choose"
        causedBy attrs
        targets characters \c ->
          phaseModifier attrs c (AttackBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf pid) 3)
      pure e
    _ -> UnseenStrike <$> runMessage msg attrs
