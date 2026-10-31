module LordOfTheRings.Treachery.Cards.KillThem (killThem) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype KillThem = KillThem TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

killThem :: TreacheryCard KillThem
killThem = treachery KillThem Cards.killThem

-- | When Revealed: Search the encounter deck, discard pile and victory display for a copy
-- of Orc War Party and add it to the staging area. Shuffle the encounter deck. Either
-- remove all progress from the main quest, or each Orc War Party in the staging area makes
-- an immediate attack against the player with the highest threat. The first player
-- chooses, once the copy found is in the staging area.
instance RunMessage KillThem where
  runMessage msg t@(KillThem attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      searchEncounterCardsAndVictoryDisplay (cardIs Enemies.orcWarParty) addToStaging
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      mains <- getsGame (filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)
      parties <- select (EnemyInStagingArea <> EnemyWithCardCode (toCardCode Enemies.orcWarParty))
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "killThem.choose"
        causedBy attrs
        labeled "killThem.removeProgress" $ for_ mains \q -> when (q.progress > 0) $ removeTokens attrs q Progress q.progress
        unless (null parties) $ labeled "killThem.attack" $ withHighestThreatPlayer attrs \p ->
          for_ parties (immediateAttack p)
      pure t
    _ -> KillThem <$> runMessage msg attrs
