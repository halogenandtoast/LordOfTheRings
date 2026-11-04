module LordOfTheRings.Enemy.Cards.UrdugsElite (urdugsElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (assaultProgressLabeled)

newtype UrdugsElite = UrdugsElite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urdugsElite :: EnemyCard UrdugsElite
urdugsElite = enemy UrdugsElite Cards.urdugsElite

-- | Forced: After Urdug's Elite engages you, either exhaust a character you control, or
-- place 1 progress on The Goblins' Assault.
instance HasAbilities UrdugsElite where
  getAbilities (UrdugsElite a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | The engaged player exhausts one of his ready characters, if he has one, or places the
-- progress (FAQ 1.44: an option of "either" is taken only if it can be done in full).
-- Shadow: Exhaust a character you control.
instance RunMessage UrdugsElite where
  runMessage msg e@(UrdugsElite attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid -> do
        readied <- selectCharacters (ReadyHero <> HeroControlledBy (PlayerWithId pid)) (ReadyAlly <> AllyControlledBy (PlayerWithId pid))
        chooseOrRunOneM pid do
          questionLabeled "urdugsElite.choose"
          causedBy attrs
          targets readied exhaust
          assaultProgressLabeled attrs
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> UrdugsElite <$> runMessage msg attrs
