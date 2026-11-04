module LordOfTheRings.Enemy.Cards.SouthronOutrider (southronOutrider) where

import LordOfTheRings.EncounterSet (EncounterSet (HaradSoldiers))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (discardRandomSetAsideEnemyLabeled)

newtype SouthronOutrider = SouthronOutrider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronOutrider :: EnemyCard SouthronOutrider
southronOutrider = enemy SouthronOutrider Cards.southronOutrider

instance HasAbilities SouthronOutrider where
  getAbilities (SouthronOutrider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

{- | Forced: After Southron Outrider engages you, either place a random enemy from the set
aside Harad Soldiers set into the encounter discard pile, or Southron Outrider gets +3
attack this round. The engaged player decides; with no such enemy left set aside, it gets
the attack.
Shadow: Exhaust a character you control.
-}
instance RunMessage SouthronOutrider where
  runMessage msg e@(SouthronOutrider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "southronOutrider.choose"
          causedBy attrs
          discardRandomSetAsideEnemyLabeled "southronOutrider.discard" HaradSoldiers
          labeled "southronOutrider.attack" $ roundModifier attrs attrs.id (AttackModifier 3)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> SouthronOutrider <$> runMessage msg attrs
