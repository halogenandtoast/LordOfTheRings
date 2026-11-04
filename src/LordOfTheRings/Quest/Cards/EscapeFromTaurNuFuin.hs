module LordOfTheRings.Quest.Cards.EscapeFromTaurNuFuin (escapeFromTaurNuFuin) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype EscapeFromTaurNuFuin = EscapeFromTaurNuFuin QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Stage 4C of Lost in Mirkwood, the last.
escapeFromTaurNuFuin :: QuestCard EscapeFromTaurNuFuin
escapeFromTaurNuFuin = quest EscapeFromTaurNuFuin Cards.escapeFromTaurNuFuin

-- | This stage cannot be defeated while a player is engaged with an enemy. When this stage
-- is defeated, the players win the game: it is the last stage.
instance HasModifiersFor EscapeFromTaurNuFuin where
  getModifiersFor (EscapeFromTaurNuFuin a) = when a.flipped do
    engaged <- selectAny (EnemyEngagedWith AnyPlayer)
    when engaged $ modifySelf a [CannotBeDefeated]

-- | Forced: At the end of the round, raise each player's threat by 1 for each enemy engaged
-- with him.
instance HasAbilities EscapeFromTaurNuFuin where
  getAbilities (EscapeFromTaurNuFuin a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

enemyCard :: CardMatcher
enemyCard = CardWithType EnemyType

{- | When Revealed: Remove Searching for a Way Out from the game. Starting with the first
player, each player discards cards from the top of the encounter deck until he discards
an enemy and puts that enemy into play engaged with him.
-}
instance RunMessage EscapeFromTaurNuFuin where
  runMessage msg q@(EscapeFromTaurNuFuin attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      EscapeFromTaurNuFuin <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searching <- getsGame (filter ((== toCardCode Objectives.searchingForAWayOut) . toCardCode) . toList . (.entities.objectives))
      for_ searching removeFromGame
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (drop (n - 1) players & take 1) \pid -> do
        discardEncounterCardsUntilFor attrs (PlayerTarget pid) enemyCard
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      for_ (find (cardMatches enemyCard) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        n <- length <$> select (EnemyEngagedWith $ PlayerWithId p)
        when (n > 0) $ raiseThreat p n
      pure q
    _ -> EscapeFromTaurNuFuin <$> runMessage msg attrs
