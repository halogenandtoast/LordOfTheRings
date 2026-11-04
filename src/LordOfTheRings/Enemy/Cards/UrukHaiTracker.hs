module LordOfTheRings.Enemy.Cards.UrukHaiTracker (urukHaiTracker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (printedHitPoints)

newtype UrukHaiTracker = UrukHaiTracker EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukHaiTracker :: EnemyCard UrukHaiTracker
urukHaiTracker = enemy UrukHaiTracker Cards.urukHaiTracker

-- | Uruk-hai Tracker gets +2 attack against characters with 2 or fewer printed hit points:
-- the characters defending its attack; undefended, it attacks no character.
instance HasModifiersFor UrukHaiTracker where
  getModifiersFor (UrukHaiTracker a) = do
    defenders <- getDefendersAgainst a.id
    weak <- anyM (fmap (<= 2) . printedHitPoints) defenders
    when weak $ modifySelf a [AttackModifier 2]

-- | Shadow: Discard a non-objective attachment you control.
instance RunMessage UrukHaiTracker where
  runMessage msg e@(UrukHaiTracker attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure e
    _ -> UrukHaiTracker <$> runMessage msg attrs
