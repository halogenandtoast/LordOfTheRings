module LordOfTheRings.Location.Cards.ShadowedCorridor (shadowedCorridor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ShadowedCorridor = ShadowedCorridor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowedCorridor :: LocationCard ShadowedCorridor
shadowedCorridor = location ShadowedCorridor Cards.shadowedCorridor

-- | As an additional cost to bypass a quest card, each player must raise his threat by 2:
-- paid as the bypass is triggered, before anything answers it.
instance HasModifiersFor ShadowedCorridor where
  getModifiersFor (ShadowedCorridor a) = modified a GameTarget [BypassCostRaisesThreat 2]

-- | Shadow: If the defending character is a hero, discard all resources in its resource
-- pool. Deal the attacking enemy another shadow card for each resource discarded in this
-- way.
instance RunMessage ShadowedCorridor where
  runMessage msg l@(ShadowedCorridor a) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      discarded <- for [hid | HeroTarget hid <- defenders] \hid -> do
        n <- (.resources) <$> getHero hid
        when (n > 0) $ removeTokens a hid Resource n
        pure n
      when (sum discarded > 0) $ dealShadowCardsDuringAttack eid pid defenders (sum discarded)
      pure l
    _ -> ShadowedCorridor <$> runMessage msg a
