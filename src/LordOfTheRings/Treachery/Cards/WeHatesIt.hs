module LordOfTheRings.Treachery.Cards.WeHatesIt (weHatesIt) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (gollumEnemy)
import LordOfTheRings.Trait (Trait (Condition))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeHatesIt = WeHatesIt TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

weHatesIt :: TreacheryCard WeHatesIt
weHatesIt = treachery WeHatesIt Cards.weHatesIt

instance HasModifiersFor WeHatesIt where
  getModifiersFor (WeHatesIt a) = case a.placement of
    AttachedTo host -> do
      modifySelf a [GainTrait Condition]
      stage <- getMainStage
      when (stage == 3) $ modified a host [AttackModifier 2]
    _ -> pure ()

instance RunMessage WeHatesIt where
  runMessage msg t@(WeHatesIt attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      gollums <- select gollumEnemy
      for_ (take 1 gollums) $ attachTreachery attrs.id
      pure t
    _ -> WeHatesIt <$> runMessage msg attrs
