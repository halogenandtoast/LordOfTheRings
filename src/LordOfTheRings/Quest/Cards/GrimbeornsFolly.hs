module LordOfTheRings.Quest.Cards.GrimbeornsFolly (grimbeornsFolly) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Troll))

newtype GrimbeornsFolly = GrimbeornsFolly QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grimbeornsFolly :: QuestCard GrimbeornsFolly
grimbeornsFolly = quest GrimbeornsFolly Cards.grimbeornsFolly

-- | When Revealed: Add Rob & Bob to the staging area. Then, each player must search the
-- encounter deck and discard pile for a non-unique Troll enemy and add it to the staging
-- area, if able. Shuffle the encounter deck.
-- When Rob & Bob have been defeated, the players have rescued Grimbeorn the Old from
-- certain death, and have won the game.
instance RunMessage GrimbeornsFolly where
  runMessage msg q@(GrimbeornsFolly attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      GrimbeornsFolly <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.robAndBob)
      eachPlayer \pid ->
        searchEncounterDeckAndDiscard pid attrs
          $ CardMatchAll [CardWithType EnemyType, CardWithTrait Troll, NotCard UniqueCard]
      pure q
    Destroy _ (EnemyTarget eid) -> do
      robAndBob <- selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.robAndBob))
      when robAndBob winGame
      GrimbeornsFolly <$> runMessage msg attrs
    _ -> GrimbeornsFolly <$> runMessage msg attrs
