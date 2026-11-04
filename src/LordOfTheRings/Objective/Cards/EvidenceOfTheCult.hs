module LordOfTheRings.Objective.Cards.EvidenceOfTheCult (evidenceOfTheCult) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype EvidenceOfTheCult = EvidenceOfTheCult ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy).
evidenceOfTheCult :: ObjectiveCard EvidenceOfTheCult
evidenceOfTheCult = objective EvidenceOfTheCult Cards.evidenceOfTheCult

{- | Attached enemy gets +1 Threat, +1 Attack, and +1 Defense: the enemy guarding it.
When Evidence of the Cult is free of encounters, add it to the victory display: the game
finds it free as it would an objective the first player claims, as Dragon Sign's.
-}
instance HasModifiersFor EvidenceOfTheCult where
  getModifiersFor (EvidenceOfTheCult a) = do
    modifySelf a [FirstPlayerClaimsWhenFree]
    case a.placement of
      AttachedTo host@(EnemyTarget _) -> modified a host bonus
      _ -> pure ()
    for_ a.objectiveGuard \cid -> do
      es <- getsGame (toList . (.enemies) . gameEntities)
      for_ (filter ((== cid) . (.id) . toCard) es) \e -> modified a (toTarget e) bonus
   where
    bonus = [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]

-- | Then, place 1 resource on the main quest.
instance RunMessage EvidenceOfTheCult where
  runMessage msg o@(EvidenceOfTheCult attrs) = case msg of
    ClaimFreeObjective oid | oid == attrs.objectiveId -> do
      addToVictoryDisplay attrs
      traverse_ (\q -> placeResources attrs q 1) =<< getMainQuests
      pure o
    _ -> EvidenceOfTheCult <$> runMessage msg attrs
