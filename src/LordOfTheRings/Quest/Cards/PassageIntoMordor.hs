module LordOfTheRings.Quest.Cards.PassageIntoMordor (passageIntoMordor) where

import LordOfTheRings.Card (CardType (EncounterSideQuestType, LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains.Helpers

newtype PassageIntoMordor = PassageIntoMordor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

passageIntoMordor :: QuestCard PassageIntoMordor
passageIntoMordor = quest PassageIntoMordor Cards.passageIntoMordor

-- | Event cards cannot shuffle cards from a player's discard pile into his deck.
instance HasModifiersFor PassageIntoMordor where
  getModifiersFor (PassageIntoMordor a) = dwindlingSuppliesModifiers a

-- | Forced: At the end of the round, each player discards X cards from the top of his
-- deck, where X is 1 more than the number of quest cards in play. If a player has no cards
-- left in his deck, he is eliminated from the game.
instance HasAbilities PassageIntoMordor where
  getAbilities (PassageIntoMordor a) = dwindlingSupplies a

{- | Setup: Each player with more than 40 cards in his deck discards cards from the top of
his deck until he has 40 cards left. The first player adds a copy of Torech Gorgor Patrol
to the staging area. Each other player adds 1 different location to the staging area.
Shuffle the encounter deck and discard cards from the top until a side quest is
discarded. Reveal that side quest. Shuffle the encounter discard pile into the encounter
deck.

Each other player in turn searches for a location unlike those already in the staging
area, as Stranded in Umbar's setup does. The deck is read for the side quest once it has
been shuffled, a step later.
-}
instance RunMessage PassageIntoMordor where
  runMessage msg q@(PassageIntoMordor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        n <- length . (.deck) <$> getPlayer p
        when (n > 40) $ discardTopOfDeck p (n - 40)
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (find (cardMatches $ cardIs Enemies.torechGorgorPatrol) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      doStep 1 msg
      turnOver attrs
      pure q
    DoStep n (PerformSetup (isTarget attrs -> True)) | n > 0 -> do
      others <- drop 1 <$> getRemainingPlayers
      case drop (n - 1) others of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> do
          shuffleEncounterDeck
          doStep 0 (PerformSetup $ toTarget attrs)
      pure q
    DoStep 0 (PerformSetup (isTarget attrs -> True)) -> do
      discardEncounterCardsUntilThen attrs (CardWithType EncounterSideQuestType) revealFromEncounterDiscard
      shuffleEncounterDiscardIntoDeck AnyCard
      pure q
    _ -> do
      runDwindlingSupplies msg attrs
      PassageIntoMordor <$> runMessage msg attrs
