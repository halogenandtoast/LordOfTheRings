module LordOfTheRings.Treachery.Cards.ExtraVigil (extraVigil) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (cityGuard, getCityGuard)
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ExtraVigil = ExtraVigil TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

extraVigil :: TreacheryCard ExtraVigil
extraVigil = treachery ExtraVigil Cards.extraVigil

-- | Attached, it counts as a Condition attachment with the text: "City Guard gets -10
-- engagement cost."
instance HasModifiersFor ExtraVigil where
  getModifiersFor (ExtraVigil a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Condition]
      modified a host [EngagementCostModifier (-10)]
    _ -> pure ()

-- | "Response: After you attack and damage City Guard, raise your threat by 1 and discard
-- Extra Vigil": each player whose characters attacked it may.
instance HasAbilities ExtraVigil where
  getAbilities (ExtraVigil a) =
    [ anyPlayer $ mkAbility a 1 $ response $ PlayerAttackDealtDamageToWindow After ActivePlayer (EnemyWithId eid)
    | AttachedTo (EnemyTarget eid) <- [a.placement]
    ]

-- | When Revealed: Attach to City Guard.
-- Shadow: If attacking enemy is City Guard, attach Extra Vigil to it.
instance RunMessage ExtraVigil where
  runMessage msg t@(ExtraVigil attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.id) =<< getCityGuard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> cityGuard) $ attachShadowTreachery eid cid eid
      pure t
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      encounterRaisesThreat pid 1
      discard attrs
      pure t
    _ -> ExtraVigil <$> runMessage msg attrs
