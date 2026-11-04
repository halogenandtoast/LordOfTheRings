module LordOfTheRings.Enemy.Cards.TheGreatGoblin (theGreatGoblin) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype TheGreatGoblin = TheGreatGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGreatGoblin :: EnemyCard TheGreatGoblin
theGreatGoblin = enemy TheGreatGoblin Cards.theGreatGoblin

-- | Forced: After The Great Goblin attacks, discard X cards from the encounter deck where X
-- is the number of players in the game. Add each Goblin enemy discarded by this effect to
-- the staging area.
instance HasAbilities TheGreatGoblin where
  getAbilities (TheGreatGoblin a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage TheGreatGoblin where
  runMessage msg e@(TheGreatGoblin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs . length =<< getRemainingPlayers
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ cards \c -> when (cdCardType (toCardDef c) == EnemyType && cardMatches (CardWithTrait Goblin) c) $ addToStagingFromDiscard c
      pure e
    _ -> TheGreatGoblin <$> runMessage msg attrs
