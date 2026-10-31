module LordOfTheRings.Enemy.Cards.ShadowDweller (shadowDweller) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (caveTorchInPlay)

newtype ShadowDweller = ShadowDweller EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowDweller :: EnemyCard ShadowDweller
shadowDweller = enemy ShadowDweller Cards.shadowDweller

-- | Shadow Dweller does not make engagement checks while Cave Torch is in play.
instance HasModifiersFor ShadowDweller where
  getModifiersFor (ShadowDweller a) = whenM caveTorchInPlay $ modifySelf a [NoEngagementChecks]

-- | Shadow: Shuffle Shadow Dweller back into the encounter deck.
instance RunMessage ShadowDweller where
  runMessage msg e@(ShadowDweller attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      shuffleShadowCardIntoEncounterDeck eid (MkEncounterCard cid (toCardCode attrs))
      pure e
    _ -> ShadowDweller <$> runMessage msg attrs
