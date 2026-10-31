module LordOfTheRings.Location.Cards.TreacherousBluffs (treacherousBluffs) where

import LordOfTheRings.Keyword (Keyword (Ambush))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoadToRivendell.Helpers (getArwen)

newtype TreacherousBluffs = TreacherousBluffs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

treacherousBluffs :: LocationCard TreacherousBluffs
treacherousBluffs = location TreacherousBluffs Cards.treacherousBluffs

-- | Forced: When an enemy with the ambush keyword engages a player, Treacherous Bluffs gets
-- +2 [Threat] until the end of the phase.
instance HasAbilities TreacherousBluffs where
  getAbilities (TreacherousBluffs a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow When]

-- | Shadow: Excess damage dealt by this attack must be assigned to Arwen Undómiel.
instance RunMessage TreacherousBluffs where
  runMessage msg l@(TreacherousBluffs a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      ambushers <- filterM (\e -> selectAny (EnemyWithId e <> EnemyWithKeyword Ambush)) [e | Window _ (EnemyEngaged e _) <- ws]
      when (notNull ambushers) $ phaseModifier a a (ThreatModifier 2)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      arwen <- getArwen
      for_ (take 1 arwen) \arwen' -> attackModifier a eid (ExcessDamageTo $ toTarget arwen')
      pure l
    _ -> TreacherousBluffs <$> runMessage msg a
