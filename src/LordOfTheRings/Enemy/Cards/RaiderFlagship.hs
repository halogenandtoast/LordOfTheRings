module LordOfTheRings.Enemy.Cards.RaiderFlagship (raiderFlagship) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype RaiderFlagship = RaiderFlagship EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

raiderFlagship :: EnemyCard RaiderFlagship
raiderFlagship = enemy RaiderFlagship Cards.raiderFlagship

-- | Boarding 1. Immune to player card effects. Only Ship objectives can be declared as
-- attackers against Raider Flagship.
instance HasModifiersFor RaiderFlagship where
  getModifiersFor (RaiderFlagship a) = modified a a [ImmuneToPlayerCardEffects, OnlyAttackedByShipObjectives]

-- | Forced: At the beginning of the encounter phase, the engaged player reveals the top
-- card of the Corsair deck and engages it.
instance HasAbilities RaiderFlagship where
  getAbilities (RaiderFlagship a) =
    [restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer) $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage RaiderFlagship where
  runMessage msg e@(RaiderFlagship attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      case attrs.placement of
        EngagedWith pid -> revealFromCorsairDeck attrs pid
        _ -> pure ()
      pure e
    _ -> RaiderFlagship <$> runMessage msg attrs
