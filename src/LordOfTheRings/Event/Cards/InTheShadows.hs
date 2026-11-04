module LordOfTheRings.Event.Cards.InTheShadows (inTheShadows) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Hobbit, Ranger))

newtype InTheShadows = InTheShadows EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, Entity)

-- | Combat Action: Each enemy engaged with you with engagement cost higher than your threat
-- gets -1 attack and -1 defense until the end of the phase.
inTheShadows :: EventCard InTheShadows
inTheShadows =
  eventWith InTheShadows Cards.inTheShadows \a ->
    a & criteriaL .~ (DuringPhase CombatPhase <> exists (shadowed a.owner))

shadowed :: PlayerId -> EnemyMatcher
shadowed pid = EnemyEngagedWith (PlayerWithId pid) <> EnemyWithEngagementCostAboveThreatOf pid

-- | Lower the cost to play In the Shadows by 1 for each Hobbit or Ranger hero you control.
instance HasModifiersFor InTheShadows where
  getModifiersFor (InTheShadows a) = do
    n <- length <$> select (HeroControlledBy (PlayerWithId a.owner) <> HeroMatchAny [HeroWithTrait Hobbit, HeroWithTrait Ranger])
    when (n > 0) $ modifySelf a [CostModifier (negate n)]

instance RunMessage InTheShadows where
  runMessage msg e@(InTheShadows attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (shadowed pid)
      for_ enemies \eid -> phaseModifiers attrs eid [AttackModifier (-1), DefenseModifier (-1)]
      pure e
    _ -> InTheShadows <$> runMessage msg attrs
