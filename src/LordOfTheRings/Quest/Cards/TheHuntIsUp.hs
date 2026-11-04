module LordOfTheRings.Quest.Cards.TheHuntIsUp (theHuntIsUp) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageAnActiveLocation)

newtype TheHuntIsUp = TheHuntIsUp QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theHuntIsUp :: QuestCard TheHuntIsUp
theHuntIsUp = quest TheHuntIsUp Cards.theHuntIsUp

-- | During the travel phase, the players must travel to a location, if able.
instance HasModifiersFor TheHuntIsUp where
  getModifiersFor (TheHuntIsUp a) = when a.flipped do
    locations <- select LocationInStagingArea
    for_ locations \l -> modified a l [MustBeTraveledTo]

-- | Forced: After a location enters play, place 1 damage on it.
-- Forced: After an enemy engages a player, place 1 damage on the active location, if able.
instance HasAbilities TheHuntIsUp where
  getAbilities (TheHuntIsUp a) =
    [ability | a.flipped, ability <- [mkAbility a 1 $ forced $ LocationEntersPlayWindow After, mkAbility a 2 $ forced $ AnyEnemyEngagedWindow After]]

{- | When Revealed: Each player searches the encounter deck and discard pile for an enemy and
adds it to the staging area. One of those enemies must be Great Warg Chief, if able. Shuffle
the encounter deck (each search does). Each goes to the staging area whoever finds it, so the first player
finds Great Warg Chief, while it is there to be found.
-}
instance RunMessage TheHuntIsUp where
  runMessage msg q@(TheHuntIsUp attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheHuntIsUp <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let chief = cardIs Enemies.greatWargChief
          findable = any (cardMatches chief) (scenario.encounterDeck <> scenario.encounterDiscard)
      players <- getRemainingPlayers
      for_ (zip [0 :: Int ..] players) \(i, pid) ->
        searchEncounterDeckAndDiscard pid attrs $ if i == 0 && findable then chief else CardWithType EnemyType
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [lid | Window _ (EntersStagingArea (LocationTarget lid)) <- ws] \lid -> placeDamage attrs lid 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      for_ [() | Window _ (EnemyEngaged _ _) <- ws] \_ -> damageAnActiveLocation attrs 1
      pure q
    _ -> TheHuntIsUp <$> runMessage msg attrs
