module LordOfTheRings.Quest.Cards.GoblinAmbush (goblinAmbush) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype GoblinAmbush = GoblinAmbush QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Into the Pit's nightmare stage 2, in place of Goblin Patrol.
goblinAmbush :: QuestCard GoblinAmbush
goblinAmbush = quest GoblinAmbush Cards.goblinAmbush

-- | X is 1 plus the number of players in the game.
instance HasModifiersFor GoblinAmbush where
  getModifiersFor (GoblinAmbush a) = do
    players <- length <$> getRemainingPlayers
    modified a a [QuestPointModifier players]

-- | Forced: After an enemy is defeated, place 1 progress on this stage.
instance HasAbilities GoblinAmbush where
  getAbilities (GoblinAmbush a) = [mkAbility a 1 $ forced $ EnemyDefeatedWindow After | a.flipped]

-- | When Revealed: The first player must search the encounter deck and discard pile for X
-- Orc enemies and add them to the staging area, where X is 1 plus the number of players
-- in the game. One of these choices must be Patrol Sentry and one must be Patrol Leader,
-- if able.
-- Progress cannot be placed on Goblin Ambush except by the above effect.
instance RunMessage GoblinAmbush where
  runMessage msg q@(GoblinAmbush attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      GoblinAmbush <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      x <- (+ 1) . length <$> getRemainingPlayers
      scenario <- getsGame (toAttrs . (.scenario))
      let available def = any ((== toCardCode def) . toCardCode) (scenario.encounterDeck <> scenario.encounterDiscard)
          musts = [cardIs def | def <- [Enemies.patrolSentry, Enemies.patrolLeader], available def]
          orc = CardWithType EnemyType <> CardWithTrait Orc
      for_ (take x $ musts <> repeat orc) (searchEncounterDeckAndDiscard lead attrs)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress ambush attrs 1
      pure q
    PlaceTokens source (isTarget attrs -> True) Progress _ | source /= ambush -> pure q
    _ -> GoblinAmbush <$> runMessage msg attrs
   where
    ambush = AbilitySource (toSource attrs) 1
