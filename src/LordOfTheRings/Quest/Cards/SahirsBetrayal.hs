module LordOfTheRings.Quest.Cards.SahirsBetrayal (sahirsBetrayal) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterBan)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait (Undead))

newtype SahirsBetrayal = SahirsBetrayal QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsBetrayal :: QuestCard SahirsBetrayal
sahirsBetrayal = quest SahirsBetrayal Cards.sahirsBetrayal

-- | While an Underwater location is the active location, players cannot play attachments
-- or allies. The players win as Shrine to Morgoth goes to the victory display.
instance HasModifiersFor SahirsBetrayal where
  getModifiersFor (SahirsBetrayal a) = when a.flipped $ underwaterBan a

{- | When Revealed: Make the set aside Shrine to Morgoth the active location, Grotto side
faceup. Captain Sahír flips to his enemy side (removing all tokens) and makes an immediate
attack against the first player. Remove Captain Sahír from the game. Each player searches
the encounter deck and discard pile for an Undead enemy and adds it to the staging area.
Shuffle the encounter deck.

His enemy side keeps the id of his objective-ally side as it turns over.
-}
instance RunMessage SahirsBetrayal where
  runMessage msg q@(SahirsBetrayal attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SahirsBetrayal <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (`placeLocation` StagingArea) =<< select IsActiveLocation
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ putIntoPlayAsActiveLocation $ filter (cardMatches $ cardIs Locations.shrineToMorgothTheDrownedRuins) aside
      sahir <- getsGame (find ((== toCardCode Objectives.captainSahirObjective) . toCardCode) . toList . (.entities.allies))
      for_ sahir \s -> do
        lead <- getLead
        -- Nightmare: he attacks each player in turn order instead of only the first player
        nightmare <- inNightmareMode
        players <- if nightmare then getRemainingPlayers else pure [lead]
        let eid = EnemyId (unAllyId $ toId s)
        flipOver (toId s)
        for_ players \p -> immediateAttack p eid
        removeFromGame eid
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs (CardWithType EnemyType <> CardWithTrait Undead)
      pure q
    _ -> SahirsBetrayal <$> runMessage msg attrs
