module LordOfTheRings.Treachery.Cards.SwishSmackWhipCrack (swishSmackWhipCrack) where

import LordOfTheRings.Trait (Trait (Goblin))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SwishSmackWhipCrack = SwishSmackWhipCrack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swishSmackWhipCrack :: TreacheryCard SwishSmackWhipCrack
swishSmackWhipCrack = treachery SwishSmackWhipCrack Cards.swishSmackWhipCrack

instance RunMessage SwishSmackWhipCrack where
  runMessage msg t@(SwishSmackWhipCrack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        staged <- length <$> select (EnemyWithTrait Goblin <> EnemyInStagingArea)
        engaged <- length <$> select (EnemyWithTrait Goblin <> EnemyEngagedWith (PlayerWithId p))
        let n = staged + engaged
        when (n > 0) $ dealDamageDivided attrs p n
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure t
    _ -> SwishSmackWhipCrack <$> runMessage msg attrs
