module LordOfTheRings.Enemy.Cards.RangingGoblin (rangingGoblin) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RangingGoblin = RangingGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rangingGoblin :: EnemyCard RangingGoblin
rangingGoblin = enemy RangingGoblin Cards.rangingGoblin

-- | Forced: After a character leaves play, shuffle Ranging Goblin and the top card in the
-- encounter discard pile back into the encounter deck.
instance HasAbilities RangingGoblin where
  getAbilities (RangingGoblin a) = [mkAbility a 1 $ forced $ CharacterLeftPlayWindow After AnyCard]

-- | When Revealed: Each player must deal 1 damage to a hero he controls.
instance RunMessage RangingGoblin where
  runMessage msg e@(RangingGoblin attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "rangingGoblin.damage"
          causedBy attrs
          targets heroes \h -> dealDamage attrs h 1
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      traverse_ moveToTopOfEncounterDeck . headMay =<< getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      shuffleIntoEncounterDeck attrs
      pure e
    _ -> RangingGoblin <$> runMessage msg attrs
