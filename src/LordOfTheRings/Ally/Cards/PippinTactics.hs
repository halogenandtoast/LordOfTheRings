module LordOfTheRings.Ally.Cards.PippinTactics (pippinTactics) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype PippinTactics = PippinTactics AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

pippinTactics :: AllyCard PippinTactics
pippinTactics = ally PippinTactics Cards.pippinTactics

-- | Pippin gets +2 attack while attacking an enemy with an engagement cost higher than
-- your threat.
instance HasModifiersFor PippinTactics where
  getModifiersFor (PippinTactics a) = for_ (placementController a.placement) \pid ->
    modifySelf a [AttackBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf pid) 2]

-- | Response: After you engage an enemy with an engagement cost higher than your threat,
-- ready Pippin. An enemy engaging you is you engaging it (FAQ 1.49).
instance HasAbilities PippinTactics where
  getAbilities (PippinTactics a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly)
        $ response
        $ MatchingEnemyEngagedWindow After (EnemyEngagedWith (PlayerWithId pid) <> EnemyWithEngagementCostAboveThreatOf pid)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage PippinTactics where
  runMessage msg s@(PippinTactics a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> PippinTactics <$> runMessage msg a
