module LordOfTheRings.Enemy.Cards.UrukHaiCaptain (urukHaiCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (defendersWeakerThan)

newtype UrukHaiCaptain = UrukHaiCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukHaiCaptain :: EnemyCard UrukHaiCaptain
urukHaiCaptain = enemy UrukHaiCaptain Cards.urukHaiCaptain

-- | Toughness 2. Cannot have attachments. Allies with fewer printed hit points than Uruk-hai
-- Captain cannot defend against Uruk-hai Captain.
instance HasModifiersFor UrukHaiCaptain where
  getModifiersFor (UrukHaiCaptain a) = do
    modifySelf a [CannotHaveAttachments]
    allies <- select AnyAlly
    weaker <- defendersWeakerThan a.id (map toTarget allies)
    for_ weaker \al -> modified a al [CannotDefendAgainst (EnemyWithId a.id)]

-- | Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage UrukHaiCaptain where
  runMessage msg e@(UrukHaiCaptain attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> UrukHaiCaptain <$> runMessage msg attrs
