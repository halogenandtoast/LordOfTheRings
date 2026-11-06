module LordOfTheRings.Location.Cards.BelfalasShipyard (belfalasShipyard) where

import LordOfTheRings.Epic.Types (EpicEffect (EpicUpgradeShip), EpicTargetQuery (EpicShip))
import LordOfTheRings.Keyword (Keyword (Pillage))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype BelfalasShipyard = BelfalasShipyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

belfalasShipyard :: LocationCard BelfalasShipyard
belfalasShipyard = location BelfalasShipyard Cards.belfalasShipyard

instance HasAbilities BelfalasShipyard where
  getAbilities (BelfalasShipyard a) =
    [ restricted a 1 (EpicTargetsExist EpicShip) $ ResponseAbility (ExploredWindow After a.id) (GroupResourcesCost Neutral 5)
    | a.placement == ActiveLocation
    ]

instance RunMessage BelfalasShipyard where
  runMessage msg l@(BelfalasShipyard attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseEpicTarget attrs pid EpicShip EpicUpgradeShip
      pure l
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      roundModifier (CardIdSource cid) eid (GainKeyword $ Pillage 3)
      pure l
    _ -> BelfalasShipyard <$> runMessage msg attrs
