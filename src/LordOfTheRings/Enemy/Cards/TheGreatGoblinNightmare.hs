module LordOfTheRings.Enemy.Cards.TheGreatGoblinNightmare (theGreatGoblinNightmare) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype TheGreatGoblinNightmare = TheGreatGoblinNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGreatGoblinNightmare :: EnemyCard TheGreatGoblinNightmare
theGreatGoblinNightmare = enemy TheGreatGoblinNightmare Cards.theGreatGoblinNightmare

{- | Forced: After The Great Goblin attacks, discard X cards from the encounter deck where X
is the number of players in the game. Add each Goblin enemy discarded by this effect to
the staging area.
-}
instance HasAbilities TheGreatGoblinNightmare where
  getAbilities (TheGreatGoblinNightmare a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage TheGreatGoblinNightmare where
  runMessage msg e@(TheGreatGoblinNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs . length =<< getRemainingPlayers
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ cards \c ->
        when (cdCardType (toCardDef c) == EnemyType && cardMatches (CardWithTrait Goblin) c)
          $ addToStagingFromDiscard c
      pure e
    _ -> TheGreatGoblinNightmare <$> runMessage msg attrs
