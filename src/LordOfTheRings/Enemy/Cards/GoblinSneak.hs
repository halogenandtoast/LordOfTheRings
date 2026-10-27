module LordOfTheRings.Enemy.Cards.GoblinSneak (goblinSneak) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (TreacheryType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype GoblinSneak = GoblinSneak EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinSneak :: EnemyCard GoblinSneak
goblinSneak = enemy GoblinSneak Cards.goblinSneak

-- | Forced: After Goblin Sneak engages a player, discard the top card of the encounter
-- deck. If it is a treachery card, Goblin Sneak engages the next player, if able.
instance HasAbilities GoblinSneak where
  getAbilities (GoblinSneak a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Add Goblin Sneak to the staging area.
instance RunMessage GoblinSneak where
  runMessage msg e@(GoblinSneak attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards
      | any ((== TreacheryType) . cdCardType . toCardDef) cards
      , EngagedWith pid <- attrs.placement -> do
          players <- getRemainingPlayers
          -- the next player clockwise; alone, there is no one else to engage
          for_ (headMay $ drop 1 $ dropWhile (/= pid) (players <> players)) \next ->
            when (next /= pid) $ engageEnemy next attrs.id
          pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> GoblinSneak <$> runMessage msg attrs
