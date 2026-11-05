module LordOfTheRings.Treachery.Cards.ArrowsThickInTheAir (arrowsThickInTheAir) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (canPlaceUnderTheBlackGate, haradEnemy, placeUnderTheBlackGate)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ArrowsThickInTheAir = ArrowsThickInTheAir TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

arrowsThickInTheAir :: TreacheryCard ArrowsThickInTheAir
arrowsThickInTheAir = treachery ArrowsThickInTheAir Cards.arrowsThickInTheAir

{- | When Revealed: Each Harad enemy gains Archery 2 until the end of the round.
Shadow: Either place attacking enemy facedown under The Black Gate after this attack, or it
makes an additional attack against you after this one. The defending player chooses.
-}
instance RunMessage ArrowsThickInTheAir where
  runMessage msg t@(ArrowsThickInTheAir attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      harad <- select haradEnemy
      for_ harad \e -> roundModifier attrs e (GainKeyword $ Archery 2)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      allowed <- canPlaceUnderTheBlackGate eid
      chooseOrRunOneM pid do
        questionLabeled "arrowsThickInTheAir.choose"
        causedBy attrs
        when allowed $ labeled "arrowsThickInTheAir.blackGate" $ doStep 2 msg
        labeled "arrowsThickInTheAir.attack" $ additionalAttack pid eid
      pure t
    DoStep 2 shadow@(ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      afterAttack eid [DoStep 1 shadow]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      placeUnderTheBlackGate eid
      pure t
    _ -> ArrowsThickInTheAir <$> runMessage msg attrs
