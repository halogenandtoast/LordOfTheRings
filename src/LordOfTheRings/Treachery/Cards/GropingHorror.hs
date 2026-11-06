module LordOfTheRings.Treachery.Cards.GropingHorror (gropingHorror) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype GropingHorror = GropingHorror TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gropingHorror :: TreacheryCard GropingHorror
gropingHorror = treachery GropingHorror Cards.gropingHorror

instance HasModifiersFor GropingHorror where
  getModifiersFor (GropingHorror a) = do
    time <- currentNight
    when (time == Just Midnight) $ modifySelf a [GainKeyword Surge]

instance RunMessage GropingHorror where
  runMessage msg t@(GropingHorror a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      players <- getRemainingPlayers
      -- Snapshot every original hand before any card is passed. Card ownership
      -- remains unchanged when GiveFromHand moves it to another player's hand.
      hands <- for players \pid -> (pid,) . (.hand) <$> getPlayer pid
      recipients <- traverse getNextPlayer players
      chooseOneM lead do
        questionLabeled "erech.gropingHorror"
        causedBy a
        labeled "erech.skipTravel" $ createEffect a GameTarget [SkipPhase TravelPhase] UntilSpent
        labeled "erech.passHands" $ case players of
          [pid] -> discardHand pid
          _ -> for_ (zip hands recipients) \((pid, hand), receiver) ->
            for_ receiver \next -> for_ hand \card -> push $ GiveFromHand pid card.id next
      pure t
    _ -> GropingHorror <$> runMessage msg a
