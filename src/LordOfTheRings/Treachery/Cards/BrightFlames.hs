module LordOfTheRings.Treachery.Cards.BrightFlames (brightFlames) where

import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (sideQuestX)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BrightFlames = BrightFlames TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brightFlames :: TreacheryCard BrightFlames
brightFlames = treachery BrightFlames Cards.brightFlames

{- | When Revealed: Each player assigns X damage among characters he controls, where X is 1
more than the number of side quests in the victory display.
Shadow: Attacking enemy gets +1 [attack] for each side quest in the victory display.
-}
instance RunMessage BrightFlames where
  runMessage msg t@(BrightFlames attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- sideQuestX
      eachPlayer \p -> dealDamageDivided attrs p x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- getSideQuestsInVictoryDisplay
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure t
    _ -> BrightFlames <$> runMessage msg attrs
