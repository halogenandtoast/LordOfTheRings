module LordOfTheRings.Ally.Cards.WatcherOfTheBruinen (watcherOfTheBruinen) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype WatcherOfTheBruinen = WatcherOfTheBruinen AllyAttrs
  deriving newtype (Show, Eq, ToJSON, Entity)
  deriving anyclass IsAlly

-- | Sentinel, as its card def says.
watcherOfTheBruinen :: AllyCard WatcherOfTheBruinen
watcherOfTheBruinen = ally WatcherOfTheBruinen Cards.watcherOfTheBruinen

-- | Watcher of the Bruinen does not exhaust to defend.
instance HasModifiersFor WatcherOfTheBruinen where
  getModifiersFor (WatcherOfTheBruinen a) = modifySelf a [DoesNotExhaustToDefend]

-- | Forced: After Watcher of the Bruinen defends, either discard it from play or discard
-- 1 card from your hand.
instance HasAbilities WatcherOfTheBruinen where
  getAbilities (WatcherOfTheBruinen a) = [mkAbility a 1 $ forced $ defendedAttack a]

instance RunMessage WatcherOfTheBruinen where
  runMessage msg s@(WatcherOfTheBruinen a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      hand <- (.hand) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "discardItOrFromHand"
        causedBy a
        labeled "discardItFromPlay" $ discard a a
        unless (null hand) $ labeled "discardOneFromHand" $ chooseAndDiscardFromHand pid a 1
      pure s
    _ -> WatcherOfTheBruinen <$> runMessage msg a
