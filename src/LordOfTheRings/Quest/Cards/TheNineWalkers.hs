module LordOfTheRings.Quest.Cards.TheNineWalkers (theNineWalkers) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageAnActiveLocation)

newtype TheNineWalkers = TheNineWalkers QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theNineWalkers :: QuestCard TheNineWalkers
theNineWalkers = quest TheNineWalkers Cards.theNineWalkers

-- | During the travel phase, the players must travel to a location, if able.
instance HasModifiersFor TheNineWalkers where
  getModifiersFor (TheNineWalkers a) = when a.flipped do
    locations <- select LocationInStagingArea
    for_ locations \l -> modified a l [MustBeTraveledTo]

-- | Forced: After an enemy engages a player, place 1 damage on the active location, if able.
instance HasAbilities TheNineWalkers where
  getAbilities (TheNineWalkers a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.flipped]

-- | When Revealed: Make Redhorn Pass the active location. The first player reveals cards
-- from the encounter deck until there is at least X threat in the staging area. X is twice
-- the number of players in the game.
instance RunMessage TheNineWalkers where
  runMessage msg q@(TheNineWalkers attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheNineWalkers <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ putIntoPlayAsActiveLocation =<< getSetAside (cardIs Locations.redhornPass)
      doStep 1 msg
      pure q
    -- one card at a time, each resolving before the threat is counted again
    DoStep 1 inner@(WhenRevealed (isTarget attrs -> True)) -> do
      threat <- getStagingThreat
      players <- length <$> getRemainingPlayers
      scenario <- getsGame (toAttrs . (.scenario))
      when (threat < 2 * players && (notNull scenario.encounterDeck || notNull scenario.encounterDiscard)) do
        revealEncounterCard
        doStep 1 inner
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [() | Window _ (EnemyEngaged _ _) <- ws] \_ -> damageAnActiveLocation attrs 1
      pure q
    _ -> TheNineWalkers <$> runMessage msg attrs
