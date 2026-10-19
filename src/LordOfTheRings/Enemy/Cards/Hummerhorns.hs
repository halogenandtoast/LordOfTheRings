module LordOfTheRings.Enemy.Cards.Hummerhorns (hummerhorns) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Hummerhorns = Hummerhorns EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hummerhorns :: EnemyCard Hummerhorns
hummerhorns = enemy Hummerhorns Cards.hummerhorns

instance HasAbilities Hummerhorns where
  getAbilities (Hummerhorns a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage Hummerhorns where
  runMessage msg e@(Hummerhorns attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOneM pid do
          questionLabeled "hummerhorns.damage"
          causedBy attrs
          targets heroes \h -> dealDamage attrs h 5
      pure e
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      characters <- charactersControlledBy pid
      for_ characters \c -> dealDamage (EnemySource eid) c (ifUndefended defender 2 1)
      pure e
    _ -> Hummerhorns <$> runMessage msg attrs
