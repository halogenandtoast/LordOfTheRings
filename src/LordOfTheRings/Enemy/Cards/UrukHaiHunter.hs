module LordOfTheRings.Enemy.Cards.UrukHaiHunter (urukHaiHunter) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (revealEncounterCardAt)

newtype UrukHaiHunter = UrukHaiHunter EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukHaiHunter :: EnemyCard UrukHaiHunter
urukHaiHunter = enemy UrukHaiHunter Cards.urukHaiHunter

-- | Peril. When Revealed: Either choose a player to reveal an encounter card, or Uruk-hai
-- Hunter makes an immediate attack against you. The player chosen reveals it at his own
-- stage, as its "you".
instance RunMessage UrukHaiHunter where
  runMessage msg e@(UrukHaiHunter attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      players <- getRemainingPlayers
      chooseOneM you do
        questionLabeled "urukHaiHunter.choose"
        causedBy attrs
        labeled "urukHaiHunter.reveal" $ chooseOrRunOneM you do
          questionLabeled "urukHaiHunter.choosePlayer"
          causedBy attrs
          targets players revealEncounterCardAt
        labeled "urukHaiHunter.attack" $ immediateAttack you attrs.id
      pure e
    _ -> UrukHaiHunter <$> runMessage msg attrs
