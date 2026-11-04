module LordOfTheRings.Event.Cards.TasteItAgain (tasteItAgain) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TasteItAgain = TasteItAgain EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a hero you control defends an attack made by an enemy with engagement
-- cost higher than your threat, ready that hero. Until the end of the phase, that hero gets
-- +2 attack while attacking that enemy.
tasteItAgain :: EventCard TasteItAgain
tasteItAgain =
  eventWith TasteItAgain Cards.tasteItAgain \a ->
    a & windowL ?~ HeroDefendedAttackWindow After (HeroControlledBy $ PlayerWithId a.owner) (EnemyWithEngagementCostAboveThreatOf a.owner)

instance RunMessage TasteItAgain where
  runMessage msg e@(TasteItAgain attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      hostile <- filterM (\(_, eid) -> selectAny (EnemyWithId eid <> EnemyWithEngagementCostAboveThreatOf pid))
        [(hid, eid) | Window _ (DefendedAttack (HeroTarget hid) eid) <- windows]
      ours <- filterM (\(hid, _) -> selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId pid))) hostile
      for_ (take 1 ours) \(hid, eid) -> do
        ready hid
        phaseModifier attrs hid (AttackBonusAgainstMatching (EnemyWithId eid) 2)
      pure e
    _ -> TasteItAgain <$> runMessage msg attrs
