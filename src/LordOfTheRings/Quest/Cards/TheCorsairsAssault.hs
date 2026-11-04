module LordOfTheRings.Quest.Cards.TheCorsairsAssault (theCorsairsAssault) where

import LordOfTheRings.Card (CardType (LocationType), EncounterCard, cdCardType, cdKeywords, cdUnique, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Aflame))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Corsair))

newtype TheCorsairsAssault = TheCorsairsAssault QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCorsairsAssault :: QuestCard TheCorsairsAssault
theCorsairsAssault = quest TheCorsairsAssault Cards.theCorsairsAssault

-- | Forced: At the end of the combat phase, deal X damage to the Aflame location in play
-- with the lowest Aflame value. X is the number of Corsair enemies in play.
instance HasAbilities TheCorsairsAssault where
  getAbilities (TheCorsairsAssault a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase | a.flipped]

{- | Setup: Set Captain Sahír and Na'asiyah aside, out of play. Add The Havens Burn,
Dream-chaser, and 1 copy of Sahír's Ravager to the staging area. Shuffle the encounter
deck and discard cards from the top of the encounter deck until X locations with the
Aflame keyword are discarded, where X is the number of players in the game. Add each
discarded Aflame location to the staging area. Then, shuffle the encounter discard pile
back into the encounter deck. The scenario set Sahír and Na'asiyah aside as it gathered
the encounter deck.
The deck is shuffled as it is gathered, and what is discarded on the way is shuffled
straight back, so the first X Aflame locations from its top are taken and the deck
shuffled: the same cards, the same deck.
-}
instance RunMessage TheCorsairsAssault where
  runMessage msg q@(TheCorsairsAssault attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      nightmare <- inNightmareMode
      let firstOf def = take 1 (filter (cardMatches $ cardIs def) deck)
          setup = concatMap firstOf [Objectives.theHavensBurn, Locations.dreamChaserLocation, Enemies.sahirsRavager]
          ships = [c | nightmare, c <- deck, cdUnique (toCardDef c), c `notElem` setup]
      for_ setup \c -> do
        removeFromEncounterDeck c
        addToStaging c
      -- Nightmare: "Instead of adding the Dream-chaser to the staging area during setup,
      -- each player must search the encounter deck for a different unique location and add
      -- it to the staging area, one of which must be the Dream-chaser." The first player's
      -- is the Dream-chaser; the others choose in turn, before the Aflame locations are
      -- discarded, so a ship left in the deck may burn.
      order <- getPlayerOrder
      searchShips (if nightmare then drop 1 order else []) ships
      TheCorsairsAssault <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let aflame c = cdCardType (toCardDef c) == LocationType && notNull [() | Aflame _ <- toList (cdKeywords $ toCardDef c)]
      for_ (take players $ filter aflame deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      corsairs <- length <$> select (EnemyWithTrait Corsair)
      dealDamageToLowestAflame attrs corsairs
      pure q
    _ -> TheCorsairsAssault <$> runMessage msg attrs
   where
    searchShips :: ReverseQueue m => [PlayerId] -> [EncounterCard] -> m ()
    searchShips (p : ps) ships = chooseOrRunOneM p do
      questionLabeled "theCorsairsAssault.searchShip"
      causedBy attrs
      targets ships \c -> do
        removeFromEncounterDeck c
        addToStaging c
        searchShips ps (filter (/= c) ships)
    searchShips [] _ = doStep 1 (Flip $ toTarget attrs)
