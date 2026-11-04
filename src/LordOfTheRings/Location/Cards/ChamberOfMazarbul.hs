module LordOfTheRings.Location.Cards.ChamberOfMazarbul (chamberOfMazarbul) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ChamberOfMazarbul = ChamberOfMazarbul LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

chamberOfMazarbul :: LocationCard ChamberOfMazarbul
chamberOfMazarbul = location ChamberOfMazarbul Cards.chamberOfMazarbul

-- | Immune to player card effects. The players cannot advance to the next stage: the
-- stage in play is not defeated while there is one after it.
instance HasModifiersFor ChamberOfMazarbul where
  getModifiersFor (ChamberOfMazarbul a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    next <- getsGame (notNull . (.questDeck) . toAttrs . (.scenario))
    when next do
      quests <- getsGame (toList . (.entities.quests))
      for_ quests \q -> modified a (toTarget q) [CannotBeDefeated]

{- | Travel: Each player searches the encounter deck and discard pile for a different enemy
and adds it to the staging area to travel here. One of those enemies must be
Orc-chieftain, if able.

The players search in turn from the first player, who finds Orc-chieftain while it is
there to be found; each finds an enemy unlike those found before him.
-}
instance RunMessage ChamberOfMazarbul where
  runMessage msg l@(ChamberOfMazarbul a) = case msg of
    TravelTo lid | lid == a.id -> do
      players <- getRemainingPlayers
      before <- select AnyEnemy
      push $ ScenarioSpecific "chamberOfMazarbul.search" (toJSON (a.id, players, before))
      ChamberOfMazarbul <$> runMessage msg a
    ScenarioSpecific "chamberOfMazarbul.search" (parseMaybe parseJSON -> Just (lid, players, before)) | lid == a.id -> do
      case players of
        [] -> pure ()
        pid : rest -> do
          scenario <- getsGame (toAttrs . (.scenario))
          found <- filter (`notElem` (before :: [EnemyId])) <$> select AnyEnemy
          codes <- traverse (fmap (CardWithCode . toCardCode) . getEnemy) found
          let chief = CardWithCode (toCardCode Enemies.orcChieftain)
              findable = any (cardMatches chief) (scenario.encounterDeck <> scenario.encounterDiscard) && null found
              different = CardWithType EnemyType <> NotCard (CardMatchAny codes)
          searchEncounterDeckAndDiscard pid a $ if findable then chief else different
          push $ ScenarioSpecific "chamberOfMazarbul.search" (toJSON (lid, rest :: [PlayerId], before))
      pure l
    _ -> ChamberOfMazarbul <$> runMessage msg a
