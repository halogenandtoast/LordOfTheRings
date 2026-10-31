module LordOfTheRings.Enemy.Cards.WickedHuorn (wickedHuorn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype WickedHuorn = WickedHuorn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

wickedHuorn :: EnemyCard WickedHuorn
wickedHuorn = enemy WickedHuorn Cards.wickedHuorn

-- | Hinder. Cannot have attachments.
instance HasModifiersFor WickedHuorn where
  getModifiersFor (WickedHuorn a) = modified a a [CannotHaveAttachments]

-- | Forced: At the beginning of each resource phase, the engaged player exhausts a hero he
-- controls.
instance HasAbilities WickedHuorn where
  getAbilities (WickedHuorn a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After ResourcePhase | EngagedWith _ <- [a.placement]]

-- | Shadow: Attacking enemy cannot take damage until the end of the round.
instance RunMessage WickedHuorn where
  runMessage msg e@(WickedHuorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      chooseAndExhaustHero attrs pid
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure e
    _ -> WickedHuorn <$> runMessage msg attrs
