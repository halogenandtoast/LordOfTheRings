module LordOfTheRings.Enemy.Cards.CrebainFromDunland (crebainFromDunland) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CrebainFromDunland = CrebainFromDunland EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crebainFromDunland :: EnemyCard CrebainFromDunland
crebainFromDunland = enemy CrebainFromDunland Cards.crebainFromDunland

-- | Forced: After Crebain from Dunland engages a player, reveal the top card of the
-- encounter deck.
instance HasAbilities CrebainFromDunland where
  getAbilities (CrebainFromDunland a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Peril. When Revealed: Either exhaust a hero you control, or engage Crebain from
-- Dunland. "You" is the player who revealed it (The Black Riders rules insert).
instance RunMessage CrebainFromDunland where
  runMessage msg e@(CrebainFromDunland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- select (ReadyHero <> HeroControlledBy (PlayerWithId you))
      chooseOrRunOneM you do
        questionLabeled "crebainFromDunland.choose"
        causedBy attrs
        unless (null heroes) $ labeled "crebainFromDunland.exhaust" $ chooseAndExhaustHero attrs you
        labeled "crebainFromDunland.engage" $ engageEnemy you attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure e
    _ -> CrebainFromDunland <$> runMessage msg attrs
