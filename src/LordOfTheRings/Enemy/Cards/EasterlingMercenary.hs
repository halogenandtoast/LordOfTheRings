module LordOfTheRings.Enemy.Cards.EasterlingMercenary (easterlingMercenary) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castlesInVictoryDisplay)

newtype EasterlingMercenary = EasterlingMercenary EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingMercenary :: EnemyCard EasterlingMercenary
easterlingMercenary = enemy EasterlingMercenary Cards.easterlingMercenary

{- | When Revealed: Either reveal an additional encounter card, or no progress can be placed
on the current quest this phase. The first player chooses for the players.
Shadow: Attacking enemy gets +1 Attack for each Castle side quest in the victory display.
-}
instance RunMessage EasterlingMercenary where
  runMessage msg e@(EasterlingMercenary attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      current <- getCurrentQuest
      chooseOneM lead do
        questionLabeled "easterlingMercenary.choose"
        causedBy attrs
        labeled "easterlingMercenary.reveal" revealEncounterCard
        labeled "easterlingMercenary.noProgress" $ for_ current \q -> phaseModifier attrs q (ProgressLimit 0)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- castlesInVictoryDisplay
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure e
    _ -> EasterlingMercenary <$> runMessage msg attrs
