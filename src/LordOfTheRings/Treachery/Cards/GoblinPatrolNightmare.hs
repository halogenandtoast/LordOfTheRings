module LordOfTheRings.Treachery.Cards.GoblinPatrolNightmare (goblinPatrolNightmare) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (captureFromHand)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GoblinPatrolNightmare = GoblinPatrolNightmare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinPatrolNightmare :: TreacheryCard GoblinPatrolNightmare
goblinPatrolNightmare = treachery GoblinPatrolNightmare Cards.goblinPatrolNightmare

-- | When Revealed: Each player may capture any number of random cards from his hand
-- underneath the current quest. Each player raises his threat by the number of cards in his
-- hand.
-- Shadow: Capture 3 random cards from your hand underneath attacking enemy.
instance RunMessage GoblinPatrolNightmare where
  runMessage msg t@(GoblinPatrolNightmare attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      quest <- getCurrentQuest
      eachPlayer \p -> do
        hand <- (.hand) <$> getPlayer p
        let cards = [pc | PlayerCard pc <- hand]
        for_ quest \q -> chooseOrRunOneM p do
          questionLabeled "goblinPatrolNightmare.capture"
          causedBy attrs
          for_ [0 .. length cards] \n -> labeledWith "goblinPatrolNightmare.captureCount" [intVar "count" n] do
            picked <- take n <$> shuffleM cards
            captureFromHand p q picked
            encounterRaisesThreat p (length cards - n)
        when (isNothing quest) $ encounterRaisesThreat p (length cards)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      hand <- (.hand) <$> getPlayer pid
      picked <- take 3 <$> shuffleM [pc | PlayerCard pc <- hand]
      captureFromHand pid (EnemyTarget eid) picked
      pure t
    _ -> GoblinPatrolNightmare <$> runMessage msg attrs
