module LordOfTheRings.Treachery.Cards.WeightOfTheRing (weightOfTheRing) where

import LordOfTheRings.Helpers.Fellowship (getSagaOneRing, ringBearer, theOneRingExhausted)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeightOfTheRing = WeightOfTheRing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Flight to the Ford burden set. Surge.
weightOfTheRing :: TreacheryCard WeightOfTheRing
weightOfTheRing = treachery WeightOfTheRing Cards.weightOfTheRing

-- | Counts as a Condition attachment with the text: "Forced: At the beginning of each quest
-- phase, either exhaust The One Ring or reveal an additional encounter card this phase."
instance HasAbilities WeightOfTheRing where
  getAbilities (WeightOfTheRing a) = case a.placement of
    AttachedTo (HeroTarget _) -> [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]
    _ -> []

-- | When Revealed: Attach to the Ring-bearer. The Ring-bearer's controller chooses for its
-- Forced effect; with The One Ring exhausted, the additional card is revealed.
instance RunMessage WeightOfTheRing where
  runMessage msg t@(WeightOfTheRing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      bearers <- select ringBearer
      for_ (headMay bearers) $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> case attrs.placement of
      AttachedTo (HeroTarget h) -> do
        controller <- (.owner) <$> getHero h
        exhausted <- theOneRingExhausted
        ring <- getSagaOneRing
        chooseOrRunOneM controller do
          questionLabeled "weightOfTheRing.choose"
          causedBy attrs
          unless exhausted $ for_ ring \r -> labeled "weightOfTheRing.exhaust" $ exhaust (ObjectiveTarget r.id)
          labeled "weightOfTheRing.reveal" $ phaseModifier attrs GameTarget (AdditionalStagingReveals 1)
        pure t
      _ -> pure t
    _ -> WeightOfTheRing <$> runMessage msg attrs
