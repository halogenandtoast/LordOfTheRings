module LordOfTheRings.Quest.Cards.BarrowDownsEscape (lostInTheFog, trappedInsideABarrow) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getGame)
import LordOfTheRings.Game.Base (allPlayers, heldEntities)
import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FogOnTheBarrowDowns.Helpers (homeQuest)
import LordOfTheRings.Trait (Trait (Wight))

newtype LostInTheFog = LostInTheFog QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheFog :: QuestCard LostInTheFog
lostInTheFog = quest LostInTheFog Cards.lostInTheFog

instance HasModifiersFor LostInTheFog where
  getModifiersFor (LostInTheFog a) = when a.flipped do
    g <- getGame
    let barrow l = toCardCode l == toCardCode Locations.greatBarrow
        outside = [p | p <- toList (allPlayers g), not p.eliminated, p.id `notElem` keys g.players]
        victory = (toAttrs g.scenario).victoryDisplay
    when (any barrow g.entities.locations || any (any barrow . (.locations)) (heldEntities g)) $
      modifySelf a [ProgressLimit 0]
    unless (null outside && any ((== toCardCode Locations.standingStones) . toCardCode) victory) $
      modifySelf a [CannotBeDefeated]

instance RunMessage LostInTheFog where
  runMessage msg q@(LostInTheFog a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      LostInTheFog <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      addSetAsideToStaging $ cardIs Locations.standingStones
      players <- getRemainingPlayers
      traverse_ revealEncounterCardAs $ filter (/= lead) players
      shuffleEncounterDiscardIntoDeck AnyCard
      shuffleSetAsideIntoEncounterDeck =<< getSetAside (cardIs Locations.greatBarrow)
      pure q
    CompleteQuest qid | qid == a.id -> q <$ winGame
    _ -> LostInTheFog <$> runMessage msg a

newtype TrappedInsideABarrow = TrappedInsideABarrow QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

trappedInsideABarrow :: QuestCard TrappedInsideABarrow
trappedInsideABarrow = quest TrappedInsideABarrow Cards.trappedInsideABarrow

instance HasModifiersFor TrappedInsideABarrow where
  getModifiersFor (TrappedInsideABarrow a) = when a.flipped do
    home <- homeQuest
    for_ home $ \q -> modifySelf a [ReturnToQuestAtTravel q.id]

instance HasAbilities TrappedInsideABarrow where
  getAbilities (TrappedInsideABarrow a) = [mkAbility a 1 $ forced $ LocationRevealedWindow When | a.flipped]

instance RunMessage TrappedInsideABarrow where
  runMessage msg q@(TrappedInsideABarrow a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      TrappedInsideABarrow <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      you <- getLead
      push $ EndQuestPhaseFor you
      discardEncounterCardsUntil a (CardWithType EnemyType <> CardWithTrait Wight)
      doStep 1 msg
      pure q
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ take 1 $ filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Wight) cards
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      you <- getLead
      enemies <- length <$> select (EnemyEngagedWith $ PlayerWithId you)
      allies <- select $ AllyControlledBy (PlayerWithId you)
      push $ DoStep (max 0 $ length allies - enemies) (ChooseAndDiscard (toSource a) you [])
      pure q
    DoStep n (ChooseAndDiscard (isSource a -> True) pid chosen) -> do
      allies <- filter (`notElem` chosen) . map AllyTarget <$> select (AllyControlledBy $ PlayerWithId pid)
      eligible <- filterM (fmap not . immuneToSource (toSource a)) allies
      if n <= 0 || null eligible
        then traverse_ (discard a) chosen
        else if n >= length eligible
          then traverse_ (discard a) (chosen <> eligible)
          else chooseOneM pid do
            questionLabeled "fog.discardBarrowAlly"
            causedBy a
            targets eligible $ \target -> push $ DoStep (n - 1) (ChooseAndDiscard (toSource a) pid (chosen <> [target]))
      pure q
    UseCardAbility pid (isSource a -> True) 1 ws -> do
      for_ [lid | Window _ (Revealed (LocationTarget lid)) <- ws] $ \lid -> do
        amount <- getLocationThreat lid
        cancelEncounterCard pid (LocationTarget lid) (discard a $ LocationTarget lid)
        encounterRaisesThreat pid amount
      pure q
    _ -> TrappedInsideABarrow <$> runMessage msg a
