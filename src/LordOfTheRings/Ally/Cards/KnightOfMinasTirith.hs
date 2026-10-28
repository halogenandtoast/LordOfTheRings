module LordOfTheRings.Ally.Cards.KnightOfMinasTirith (knightOfMinasTirith) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype KnightOfMinasTirith = KnightOfMinasTirith AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

knightOfMinasTirith :: AllyCard KnightOfMinasTirith
knightOfMinasTirith = ally KnightOfMinasTirith Cards.knightOfMinasTirith

-- | If each of your heroes has a printed tactics resource icon, Knight of Minas Tirith
-- gains: "Response: After Knight of Minas Tirith enters play, choose an enemy in the
-- staging area. Engage that enemy and exhaust Knight of Minas Tirith to declare it as an
-- attacker (and resolve its attack) against that enemy."
instance HasAbilities KnightOfMinasTirith where
  getAbilities (KnightOfMinasTirith a) =
    [ restricted a 1 (eachHeroHasPrintedIcon pid Tactics <> exists EnemyInStagingArea)
        $ response
        $ entersPlay a
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage KnightOfMinasTirith where
  runMessage msg s@(KnightOfMinasTirith a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      chooseOrRunOneM pid do
        questionLabeled "knightOfMinasTirith.chooseEnemy"
        causedBy a
        targets enemies \eid -> do
          engageEnemy pid eid
          attackEnemyWith pid a (toTarget a) eid 0
      pure s
    _ -> KnightOfMinasTirith <$> runMessage msg a
