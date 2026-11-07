module LordOfTheRings.Location.Cards.BarrowDownsLocations (stoneRing, northDownsFog) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Wight))

newtype BarrowDownsLocations = BarrowDownsLocations LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stoneRing, northDownsFog :: LocationCard BarrowDownsLocations
stoneRing = location BarrowDownsLocations Cards.stoneRing
northDownsFog = location BarrowDownsLocations Cards.northDownsFog

instance HasModifiersFor BarrowDownsLocations where
  getModifiersFor (BarrowDownsLocations a) = when (a.placement == StagingArea) $
    if toCardCode a == toCardCode Cards.stoneRing then
      modified a GameTarget [EngagedEnemiesWithTraitGain Wight [AttackModifier 1, DefenseModifier 1, EngagementModifiersApplyToEachPlayer]]
    else modified a GameTarget [WillpowerForDefenseAgainst Wight]

instance RunMessage BarrowDownsLocations where
  runMessage msg l@(BarrowDownsLocations a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId, toCardCode a == toCardCode Cards.northDownsFog -> do
      enemyAttackModifier a eid (AttackModifier 2)
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId, toCardCode a == toCardCode Cards.northDownsFog -> do
      players <- getStagePlayers
      eachPlayer \pid -> when (pid `elem` players) $ encounterRaisesThreat pid 2
      pure l
    _ -> BarrowDownsLocations <$> runMessage msg a
