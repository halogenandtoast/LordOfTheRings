module LordOfTheRings.Location.Cards.EasternShore (easternShore) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype EasternShore = EasternShore LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternShore :: LocationCard EasternShore
easternShore = location EasternShore Cards.easternShore

-- | While Eastern Shore is in the staging area, each player cannot play more than 1 event
-- each round: once he has played one this round, he can play no other while it is there.
instance HasModifiersFor EasternShore where
  getModifiersFor (EasternShore a) = when (a.placement == StagingArea) do
    effects <- getsGame gameEffects
    for_ (nub [pid | e <- effects, PlayedEventThisRound `elem` effectModifiers e, PlayerTarget pid <- [effectTarget e]]) \pid ->
      modified a (PlayerTarget pid) [CannotPlay $ CardWithType EventType]

-- | Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, remove 1
-- time counter from the current quest.
instance RunMessage EasternShore where
  runMessage msg l@(EasternShore a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      removeTimeCountersFromCurrentQuest a 1
      pure l
    _ -> EasternShore <$> runMessage msg a
