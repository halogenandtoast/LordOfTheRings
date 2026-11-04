module LordOfTheRings.Enemy.Cards.UrukHaiArcher (urukHaiArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (playersAtThisStage)

newtype UrukHaiArcher = UrukHaiArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukHaiArcher :: EnemyCard UrukHaiArcher
urukHaiArcher = enemy UrukHaiArcher Cards.urukHaiArcher

-- | Archery X. Its threat is X. X is the number of players at this stage.
instance HasModifiersFor UrukHaiArcher where
  getModifiersFor (UrukHaiArcher a) = do
    x <- length <$> playersAtThisStage
    modifySelf a [ThreatModifier x, GainKeyword (Archery x)]

-- | Peril. When Revealed: Discard an ally you control.
instance RunMessage UrukHaiArcher where
  runMessage msg e@(UrukHaiArcher attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      allies <- select (AllyControlledBy $ PlayerWithId you)
      chooseOneM you do
        questionLabeled "urukHaiArcher.discardAlly"
        causedBy attrs
        targets allies discard
      pure e
    _ -> UrukHaiArcher <$> runMessage msg attrs
