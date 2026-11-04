module LordOfTheRings.Enemy.Cards.TheKingsElite (theKingsElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TheKingsElite = TheKingsElite EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingsElite :: EnemyCard TheKingsElite
theKingsElite = enemy TheKingsElite Cards.theKingsElite

-- | Cannot have player card attachments.
instance HasModifiersFor TheKingsElite where
  getModifiersFor (TheKingsElite a) = modifySelf a [CannotHavePlayerAttachments]

-- | Forced: When The King's Elite attacks, place 1 resource on the main quest.
instance HasAbilities TheKingsElite where
  getAbilities (TheKingsElite a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage TheKingsElite where
  runMessage msg e@(TheKingsElite attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResourcesOnMainQuest attrs 1
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> TheKingsElite <$> runMessage msg attrs
