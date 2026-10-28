module LordOfTheRings.Treachery.Cards.WatcherInTheWoods (watcherInTheWoods) where

import LordOfTheRings.Helpers.Character (getCommittedCharacters)
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WatcherInTheWoods = WatcherInTheWoods TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

watcherInTheWoods :: TreacheryCard WatcherInTheWoods
watcherInTheWoods = treachery WatcherInTheWoods Cards.watcherInTheWoods

-- | When Revealed: Raise each player's threat by the number of questing characters. (If
-- the current quest has the battle or siege keyword, Watcher in the Woods gains surge.)
-- Shadow: Each player raises his threat by the number of enemies engaged with him.
instance RunMessage WatcherInTheWoods where
  runMessage msg t@(WatcherInTheWoods attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      questing <- length <$> getCommittedCharacters
      eachPlayer \pid -> encounterRaisesThreat pid questing
      war <- (||) <$> currentQuestHas Battle <*> currentQuestHas Siege
      when war revealEncounterCard
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      eachPlayer \pid -> encounterRaisesThreat pid . length =<< select (EnemyEngagedWith $ PlayerWithId pid)
      pure t
    _ -> WatcherInTheWoods <$> runMessage msg attrs
