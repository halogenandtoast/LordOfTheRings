module LordOfTheRings.Quest.Cards.RoutTheGoblins (routTheGoblins) where

import Data.Function (on)
import Data.List (nubBy)
import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType), EncounterCard, cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Name (Name)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Goblin))

newtype RoutTheGoblins = RoutTheGoblins QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
routTheGoblins :: QuestCard RoutTheGoblins
routTheGoblins = quest RoutTheGoblins Cards.routTheGoblins

{- | When Revealed: Each player searches the encounter deck and discard pile for a
different Goblin enemy and puts it into play engaged with him. Shuffle the encounter deck.
Each player in turn order chooses a Goblin enemy by its title, among those no player
before him took; copies of a title are alike, so the first found is taken, from the deck
before the discard pile.
-}
instance RunMessage RoutTheGoblins where
  runMessage msg q@(RoutTheGoblins attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      s <- getsGame (toAttrs . gameScenario)
      let goblins = filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Goblin)
          inDeck = goblins s.scenarioEncounterDeck
          found = map (,True) inDeck <> map (,False) (goblins s.scenarioEncounterDiscard)
      rout found [] =<< getRemainingPlayers
      shuffleEncounterDeck
      pure q
    _ -> RoutTheGoblins <$> runMessage msg attrs
   where
    title :: EncounterCard -> Name
    title = cdName . toCardDef
    rout :: ReverseQueue m => [(EncounterCard, Bool)] -> [Name] -> [PlayerId] -> m ()
    rout _ _ [] = pure ()
    rout found taken (p : rest) = do
      -- the first copy of each title no player before took
      let picks = nubBy ((==) `on` title . fst) [x | x@(c, _) <- found, title c `notElem` taken]
      if null picks
        then rout found taken rest
        else chooseOrRunOneM p do
          questionLabeled "routTheGoblins.choose"
          causedBy attrs
          showing (map (EncounterCard . fst) picks)
          for_ picks \(c, fromDeck) ->
            targeting c do
              if fromDeck then removeFromEncounterDeck c else removeFromEncounterDiscard c
              putEnemyIntoPlayEngaged p c
              rout found (title c : taken) rest
