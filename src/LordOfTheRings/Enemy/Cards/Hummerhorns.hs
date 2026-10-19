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
        unless (null heroes)
          $ push
          $ questionLabelWith (ikey "label.hummerhorns.damage") (Just $ toTarget attrs) [] pid [targetLabel h [DealDamage (toSource attrs) (toTarget h) 5] | h <- heroes]
      pure e
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      characters <- charactersControlledBy pid
      let damage = if isJust defender then 1 else 2
      pushAll [DealDamage (EnemySource eid) c damage | c <- characters]
      pure e
    _ -> Hummerhorns <$> runMessage msg attrs
