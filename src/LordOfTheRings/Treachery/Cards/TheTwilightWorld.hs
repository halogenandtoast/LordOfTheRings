module LordOfTheRings.Treachery.Cards.TheTwilightWorld (theTwilightWorld) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Helpers.Fellowship (getReadySagaOneRing, ringBearer)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)
import LordOfTheRings.Trait (Trait (Condition))

newtype TheTwilightWorld = TheTwilightWorld TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTwilightWorld :: TreacheryCard TheTwilightWorld
theTwilightWorld = treachery TheTwilightWorld Cards.theTwilightWorld

instance HasModifiersFor TheTwilightWorld where
  getModifiersFor (TheTwilightWorld a) = withHost a $ \_ -> modifySelf a [GainTrait Condition]

instance HasAbilities TheTwilightWorld where
  getAbilities (TheTwilightWorld a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ forced $ PhaseBeginsWindow After PlanningPhase]
    _ -> []

instance RunMessage TheTwilightWorld where
  runMessage msg t@(TheTwilightWorld a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      taken <- hostsOfTreachery (toCardCode Cards.theTwilightWorld)
      heroes <- filter (\h -> toTarget h `notElem` taken) <$> select ringBearer
      for_ (take 1 heroes) $ attachTreachery a.id
      pure t
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a $ \host -> characterController host >>= traverse_ \pid -> do
        ring <- getReadySagaOneRing
        chooseOrRunOneM pid do
          questionLabeled "theTwilightWorld.choose"
          causedBy a
          for_ ring $ \oid -> labeled "theTwilightWorld.exhaust" $ exhaust (ObjectiveTarget oid)
          labeled "theTwilightWorld.life" $ reduceRingBearersLife 1
      pure t
    _ -> TheTwilightWorld <$> runMessage msg a
