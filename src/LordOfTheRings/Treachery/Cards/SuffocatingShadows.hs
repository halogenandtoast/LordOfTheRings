module LordOfTheRings.Treachery.Cards.SuffocatingShadows (suffocatingShadows) where

import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuffocatingShadows = SuffocatingShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suffocatingShadows :: TreacheryCard SuffocatingShadows
suffocatingShadows = treachery SuffocatingShadows Cards.suffocatingShadows

-- | When Revealed: Each player assigns X damage among allies he controls, where X is 1
-- more than the number of quest cards in the victory display.
-- Shadow: Attacking enemy gets +1 attack (+3 attack instead if there are 3 or more quests
-- in the victory display).
instance RunMessage SuffocatingShadows where
  runMessage msg t@(SuffocatingShadows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- dagnirX
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        unless (null allies) $ dealDamageDividedAmong attrs p allies x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      quests <- questCardsInVictoryDisplay
      enemyGetsAttack attrs eid (if quests >= 3 then 3 else 1)
      pure t
    _ -> SuffocatingShadows <$> runMessage msg attrs
