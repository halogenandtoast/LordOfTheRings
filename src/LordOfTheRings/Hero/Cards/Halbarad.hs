module LordOfTheRings.Hero.Cards.Halbarad (halbarad) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Halbarad = Halbarad HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

halbarad :: HeroCard Halbarad
halbarad = hero Halbarad Cards.halbarad

-- | While you are engaged with any enemy, Halbarad does not exhaust to commit to a quest.
-- You may optionally engage 1 additional enemy during the encounter phase.
instance HasModifiersFor Halbarad where
  getModifiersFor (Halbarad a) = do
    modified a (PlayerTarget a.owner) [OptionallyEngagesAdditionalEnemy]
    engaged <- selectAny (EnemyEngagedWith $ PlayerWithId a.owner)
    when engaged $ modifySelf a [DoesNotExhaustToCommit]

instance RunMessage Halbarad where
  runMessage msg (Halbarad attrs) = Halbarad <$> runMessage msg attrs
