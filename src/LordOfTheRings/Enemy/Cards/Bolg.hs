module LordOfTheRings.Enemy.Cards.Bolg (bolg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Trait (Trait (Goblin))

newtype Bolg = Bolg EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

bolg :: EnemyCard Bolg
bolg = enemy Bolg Cards.bolg

-- | Immune to player card effects. X is the number of players in the game: his threat.
-- Cannot leave the staging area.
instance HasModifiersFor Bolg where
  getModifiersFor (Bolg a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ImmuneToPlayerCardEffects, ThreatModifier players, CannotLeaveStagingArea, CannotBeOptionallyEngagedBy AnyPlayer]

-- | The first Goblin revealed each round gains surge. Read off the window after it is
-- revealed, before its surge resolves, once a round.
instance HasAbilities Bolg where
  getAbilities (Bolg a) = [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ silent $ EnemyWithTraitRevealedWindow After Goblin]

instance RunMessage Bolg where
  runMessage msg e@(Bolg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [eid | Window _ (Revealed (EnemyTarget eid)) <- windows] \eid -> roundModifier attrs eid (GainKeyword Surge)
      pure e
    _ -> Bolg <$> runMessage msg attrs
