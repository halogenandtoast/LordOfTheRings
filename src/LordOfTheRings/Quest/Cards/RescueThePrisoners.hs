module LordOfTheRings.Quest.Cards.RescueThePrisoners (rescueThePrisoners) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType), cdName, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (discardUntilAlly, takeCaptive, unattachedInStaging)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait (Orc))

newtype RescueThePrisoners = RescueThePrisoners QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

rescueThePrisoners :: QuestCard RescueThePrisoners
rescueThePrisoners = quest RescueThePrisoners Cards.rescueThePrisoners

-- | This stage cannot be defeated while a guarded objective is in play.
instance HasModifiersFor RescueThePrisoners where
  getModifiersFor (RescueThePrisoners a) = when a.flipped do
    captives <- guardedCharacters
    when (notNull captives) $ modifySelf a [CannotBeDefeated]

-- | Forced: After any number of characters are rescued, reveal an encounter card.
instance HasAbilities RescueThePrisoners where
  getAbilities (RescueThePrisoners a) = [mkAbility a 1 $ forced $ CharacterRescuedWindow After Nothing | a.flipped]

{- | Setup: Set Uruk Chieftain aside, out of play. Add The Captives of Cirith Gurat and 1 copy
of Dungeon Cell to the staging area. Attach Abaan to Dungeon Cell. Each player searches the
encounter deck for a different Orc enemy and adds it to the staging area. Shuffle the
encounter deck. Starting with the first player, each player discards cards from the top of
his deck until an ally that is not in play is discarded and attaches that ally to an
unattached enemy in the staging area.

The scenario set Uruk Chieftain, the Captives, Abaan and a Dungeon Cell aside as it gathered
the encounter deck. Each player's search and discard is a step of its own, read once the
one before has resolved: steps 1 to N are the searches, N+1 to 2N the allies.
-}
instance RunMessage RescueThePrisoners where
  runMessage msg q@(RescueThePrisoners attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      let copy def = take 1 [c | c <- aside, toCardCode c == toCardCode def]
      traverse_ addToStaging (copy Objectives.theCaptivesOfCirithGurat)
      for_ (copy Locations.dungeonCell) \cell -> do
        addToStaging cell
        for_ (copy Objectives.abaan) \abaan -> guardCardWith (EncounterCard abaan) (EncounterCard cell)
      doStep 1 msg
      turnOver attrs
      pure q
    DoStep n inner@(PerformSetup (isTarget attrs -> True)) -> do
      players <- getPlayerOrder
      let seats = length players
      case drop ((n - 1) `mod` seats) players of
        (p : _)
          | n <= seats -> do
              deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
              staged <- traverse (fmap (cdName . toCardDef) . getEnemy) =<< select EnemyInStagingArea
              let orcs = [c | c <- deck, cardMatches orcEnemy c, cdName (toCardDef c) `notElem` staged]
              chooseOrRunOneM p do
                questionLabeled "rescueThePrisoners.searchOrc"
                causedBy attrs
                targets orcs \c -> do
                  removeFromEncounterDeck c
                  addToStaging c
              when (n == seats) shuffleEncounterDeck
              doStep (n + 1) inner
          | n <= 2 * seats -> do
              found <- discardUntilAlly p \c -> selectNone (AllyWithTitle $ cdName $ toCardDef c)
              for_ found \card -> takeCaptive attrs p card =<< unattachedInStaging False
              doStep (n + 1) inner
        _ -> pure ()
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    _ -> RescueThePrisoners <$> runMessage msg attrs
   where
    orcEnemy = CardWithType EnemyType <> CardWithTrait Orc
