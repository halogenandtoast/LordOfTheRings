module LordOfTheRings.Location.Cards.MordorRoad (mordorRoad) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype MordorRoad = MordorRoad LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

mordorRoad :: LocationCard MordorRoad
mordorRoad = location MordorRoad Cards.mordorRoad

-- | Mordor Road gets +1 quest point for each ally controlled by the player who controls the
-- most allies.
instance HasModifiersFor MordorRoad where
  getModifiersFor (MordorRoad a) = do
    players <- getRemainingPlayers
    most <- foldr max 0 <$> for players \p -> length <$> select (AllyControlledBy $ PlayerWithId p)
    when (most > 0) $ modifySelf a [QuestPointModifier most]

-- | Forced: After Mordor Road becomes the active location, each player discards the top
-- card of the encounter deck. Add each enemy discarded this way to the staging area.
instance HasAbilities MordorRoad where
  getAbilities (MordorRoad a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage MordorRoad where
  runMessage msg l@(MordorRoad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> discardTopOfEncounterDeckFor a (PlayerTarget pid) 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (filter (cardMatches $ CardWithType EnemyType) cards)
      pure l
    _ -> MordorRoad <$> runMessage msg a
