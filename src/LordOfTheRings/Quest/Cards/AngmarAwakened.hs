module LordOfTheRings.Quest.Cards.AngmarAwakened (angmarAwakened) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateTopOfDeck)
import LordOfTheRings.Scenario.Types (scenarioSetAside)

newtype AngmarAwakened = AngmarAwakened QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

angmarAwakened :: QuestCard AngmarAwakened
angmarAwakened = quest AngmarAwakened Cards.angmarAwakened

{- | When Revealed: Add Daechanar to the staging area. Make Altar of Midwinter the active
location, returning any previously active location to the staging area. Each player
reanimates the top card of his deck.
When Daechanar is defeated, advance to the next stage.
-}
instance RunMessage AngmarAwakened where
  runMessage msg q@(AngmarAwakened attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      -- the Sorcery cards stage 1 held off as it was defeated
      held <- getsGame \g -> [t.id | t <- toList g.entities.treacheries, t.placement == AttachedTo GameTarget]
      for_ held \tid -> attachTreachery tid attrs
      askToResolveWhenRevealed attrs
      AngmarAwakened <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.daechanar)
      traverse_ (`placeLocation` StagingArea) =<< select IsActiveLocation
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ putIntoPlayAsActiveLocation $ filter ((== toCardCode Locations.altarOfMidwinter) . toCardCode) aside
      eachPlayer \pid -> reanimateTopOfDeck pid 1
      pure q
    Destroy _ (EnemyTarget eid) -> do
      daechanar <- selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.daechanar))
      when daechanar $ completeQuest attrs.id
      AngmarAwakened <$> runMessage msg attrs
    _ -> AngmarAwakened <$> runMessage msg attrs
