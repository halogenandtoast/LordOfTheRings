module LordOfTheRings.Ally.Cards.LindonNavigator (lindonNavigator) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype LindonNavigator = LindonNavigator AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lindonNavigator :: AllyCard LindonNavigator
lindonNavigator = ally LindonNavigator Cards.lindonNavigator

-- | Lindon Navigator does not exhaust to commit to a quest, and can commit to quests while
-- exhausted.
instance HasModifiersFor LindonNavigator where
  getModifiersFor (LindonNavigator a) = modifySelf a [DoesNotExhaustToCommit, CanCommitWhileExhausted]

-- | Forced: After resolving a quest to which Lindon Navigator was committed, either discard
-- it from play or discard 1 card from your hand.
instance RunMessage LindonNavigator where
  runMessage msg s@(LindonNavigator a) = case msg of
    QuestResolution | allyCommitted a -> do
      for_ (placementController a.placement) \pid -> do
        hand <- (.hand) <$> getPlayer pid
        chooseOrRunOneM pid do
          questionLabeled "lindonNavigator.choose"
          causedBy a
          labeled "lindonNavigator.discardSelf" $ discard a a
          unless (null hand) $ labeled "lindonNavigator.discardCard" $ chooseAndDiscardFromHand pid a 1
      pure s
    _ -> LindonNavigator <$> runMessage msg a
