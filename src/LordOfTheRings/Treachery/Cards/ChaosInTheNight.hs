module LordOfTheRings.Treachery.Cards.ChaosInTheNight (chaosInTheNight) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ChaosInTheNight = ChaosInTheNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chaosInTheNight :: TreacheryCard ChaosInTheNight
chaosInTheNight = treachery ChaosInTheNight Cards.chaosInTheNight

{- | When Revealed: The first player must choose: either raise each player's threat by 3,
or progress cannot be placed on quest cards until the end of the round.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, remove all
progress from the main quest.
-}
instance RunMessage ChaosInTheNight where
  runMessage msg t@(ChaosInTheNight attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      quests <- getsGame (map toTarget . toList . (.entities.quests))
      chooseOneM lead do
        questionLabeled "chaosInTheNight.choose"
        causedBy attrs
        labeled "chaosInTheNight.threat" $ eachPlayer (`encounterRaisesThreat` 3)
        labeled "chaosInTheNight.noProgress" $ for_ quests \q -> roundModifier attrs q (ProgressLimit 0)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      removeProgressFromMainQuest attrs =<< getMainQuestProgress
      pure t
    _ -> ChaosInTheNight <$> runMessage msg attrs
