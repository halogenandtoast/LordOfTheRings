module LordOfTheRings.Enemy.Cards.Naurlhug (naurlhug) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (stalkingResources)

newtype Naurlhug = Naurlhug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

naurlhug :: EnemyCard Naurlhug
naurlhug = enemy Naurlhug Cards.naurlhug

instance HasModifiersFor Naurlhug where
  getModifiersFor (Naurlhug a) = do
    x <- stalkingResources
    -- Stage 4B removes "Cannot take damage" only while its lair is in victory.
    vulnerable <- getsGame \g ->
      any (\q -> toCardCode q == "the-ruins-of-belegost-007" && (toAttrs q).flipped) (toList g.entities.quests)
        && any ((== "the-ruins-of-belegost-021") . toCardCode) (toAttrs g.scenario).victoryDisplay
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier x] <> [CannotTakeDamage | not vulnerable]

instance HasAbilities Naurlhug where
  getAbilities (Naurlhug a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage Naurlhug where
  runMessage msg e@(Naurlhug a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      x <- stalkingResources
      for_ [pid | Window _ (EnemyAttacking _ pid) <- windows] \pid -> do
        characters <- charactersControlledBy pid
        dealDamageDividedAmong a pid characters x
      pure e
    _ -> Naurlhug <$> runMessage msg a
