module LordOfTheRings.Enemy.Cards.Murzag (murzag) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype Murzag = Murzag EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

murzag :: EnemyCard Murzag
murzag = enemy Murzag Cards.murzag

-- | Forced: After Murzag attacks, reveal the top card of the encounter deck. If that card
-- is an enemy, put it into play engaged with the defending player.
instance HasAbilities Murzag where
  getAbilities (Murzag a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | The card is revealed as any other, its When Revealed resolving; an enemy then engages
-- the defending player, read back as a later step carrying it.
-- ponytail: the enemy passes through the staging area as it is revealed, and its surge,
-- if any, resolves before it engages; an empty deck reshuffled to reveal engages nothing.
instance RunMessage Murzag where
  runMessage msg e@(Murzag attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyAttacking _ pid) <- ws]) \pid -> do
        top <- getsGame (headMay . (.encounterDeck) . toAttrs . (.scenario))
        revealEncounterCard
        for_ top \card ->
          when (cdCardType (toCardDef card) == EnemyType) $ doStep 1 (PutEnemyIntoPlayEngaged pid card)
      pure e
    DoStep 1 (PutEnemyIntoPlayEngaged pid card) -> do
      revealed <- getsGame (filter ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies))
      for_ revealed \other ->
        unless (other.placement == EngagedWith pid) $ engageEnemy pid (toId other)
      pure e
    _ -> Murzag <$> runMessage msg attrs
