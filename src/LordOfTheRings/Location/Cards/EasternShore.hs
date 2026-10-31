module LordOfTheRings.Location.Cards.EasternShore (easternShore) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype EasternShore = EasternShore LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternShore :: LocationCard EasternShore
easternShore = location EasternShore Cards.easternShore

-- | While Eastern Shore is in the staging area, each player cannot play more than 1 event
-- each round: once he plays one, he can play no other this round.
-- Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, remove 1
-- time counter from the current quest.
instance RunMessage EasternShore where
  runMessage msg l@(EasternShore a) = case msg of
    -- ponytail: the bar lasts the round even if Eastern Shore leaves the staging area after
    -- the event is played
    CheckWindows ws | a.placement == StagingArea -> do
      for_ (nub [pid | Window _ (PlayingEvent pid _) <- ws]) \pid ->
        roundModifier a (PlayerTarget pid) (CannotPlay $ CardWithType EventType)
      EasternShore <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      removeTimeCountersFromCurrentQuest a 1
      pure l
    _ -> EasternShore <$> runMessage msg a
