module LordOfTheRings.Quest.Cards.TheBattleOfTheMorannon (theBattleOfTheMorannon) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType), EncounterCard, cdName, toCardDef)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBlackGateOpens.Helpers (blackGateResources)
import LordOfTheRings.Trait (Trait (Nazgul, Orc, Troll))

newtype TheBattleOfTheMorannon = TheBattleOfTheMorannon QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfTheMorannon :: QuestCard TheBattleOfTheMorannon
theBattleOfTheMorannon = quest TheBattleOfTheMorannon Cards.theBattleOfTheMorannon

-- | Dire. During the staging step, the first player does not reveal an encounter card as
-- normal. Instead, he reveals 1 encounter card for each resource on The Black Gate.
instance HasModifiersFor TheBattleOfTheMorannon where
  getModifiersFor (TheBattleOfTheMorannon a) = when a.flipped do
    n <- blackGateResources
    modified a GameTarget [FirstPlayerRevealsForStaging n]

-- | Forced: After the players quest unsuccessfully, discard the active location.
instance HasAbilities TheBattleOfTheMorannon where
  getAbilities (TheBattleOfTheMorannon a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After | a.flipped]

{- | Dire. When Revealed: Each player searches the encounter deck and discard pile for a
different Orc, Troll, or Nazgûl enemy and adds it to the staging area. Shuffle Gwaihir
into the encounter deck. "Different" is by title: each player in turn picks one of a title
no player before him picked.

This stage cannot be defeated: it has no quest points. Each round the players survive
gives the Ring-bearer more time to fulfill the quest to Mount Doom.
-}
instance RunMessage TheBattleOfTheMorannon where
  runMessage msg q@(TheBattleOfTheMorannon attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheBattleOfTheMorannon <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      searchEach players [] [(c, True) | c <- deck] [(c, False) | c <- discarded]
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      active <- select IsActiveLocation
      for_ active discard
      pure q
    _ -> TheBattleOfTheMorannon <$> runMessage msg attrs
   where
    soldier = CardWithType EnemyType <> CardMatchAny [CardWithTrait Orc, CardWithTrait Troll, CardWithTrait Nazgul]
    title c = let Name t _ = cdName (toCardDef c) in t
    searchEach :: ReverseQueue m => [PlayerId] -> [Text] -> [(EncounterCard, Bool)] -> [(EncounterCard, Bool)] -> m ()
    searchEach (pid : rest) taken deck discarded = do
      let found = [x | x@(c, _) <- deck <> discarded, cardMatches soldier c, title c `notElem` taken]
      if null found
        then searchEach rest taken deck discarded
        else chooseOneM pid do
          questionLabeled "theBattleOfTheMorannon.search"
          causedBy attrs
          showing [EncounterCard c | (c, _) <- found]
          for_ found \(c, fromDeck) -> targeting (CardIdTarget c.id) do
            if fromDeck then removeFromEncounterDeck c else removeFromEncounterDiscard c
            addToStaging c
            searchEach rest (title c : taken) (filter ((/= c.id) . (.id) . fst) deck) (filter ((/= c.id) . (.id) . fst) discarded)
    searchEach [] _ _ _ = do
      -- the searched deck is shuffled with Gwaihir
      gwaihir <- getSetAside (cardIs Objectives.gwaihirTheBlackGateOpens)
      if null gwaihir then shuffleEncounterDeck else shuffleSetAsideIntoEncounterDeck gwaihir
