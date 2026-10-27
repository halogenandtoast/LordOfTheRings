module LordOfTheRings.Treachery.Cards.BurningLow (burningLow) where

import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch)
import LordOfTheRings.Trait (Trait (Dark))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BurningLow = BurningLow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

burningLow :: TreacheryCard BurningLow
burningLow = treachery BurningLow Cards.burningLow

-- | When Revealed: Each enemy and location currently in the staging area gets +1 [Threat]
-- until the end of the phase. (+3 [Threat] instead if it is a Dark location.) Players may
-- exhaust a Cave Torch to cancel this effect.
-- Shadow: attacking enemy gets +2 [Attack].
instance RunMessage BurningLow where
  runMessage msg t@(BurningLow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select EnemyInStagingArea
      locations <- select LocationInStagingArea
      dark <- select (LocationInStagingArea <> LocationWithTrait Dark)
      let burn :: ReverseQueue m => m ()
          burn = do
            for_ enemies \e -> phaseModifier attrs e (ThreatModifier 1)
            for_ locations \l -> phaseModifier attrs l (ThreatModifier $ if l `elem` dark then 3 else 1)
      readyCaveTorch >>= \case
        Nothing -> burn
        Just (torch, pid) -> chooseOneM pid do
          questionLabeled "burningLow.cancel"
          causedBy attrs
          labeled "burningLow.exhaustTorch" $ exhaust (ObjectiveTarget torch)
          labeled "burningLow.burn" burn
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure t
    _ -> BurningLow <$> runMessage msg attrs
