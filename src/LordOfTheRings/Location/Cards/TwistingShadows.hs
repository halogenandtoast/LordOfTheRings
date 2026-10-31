module LordOfTheRings.Location.Cards.TwistingShadows (twistingShadows) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Dark))

newtype TwistingShadows = TwistingShadows LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

twistingShadows :: LocationCard TwistingShadows
twistingShadows = location TwistingShadows Cards.twistingShadows

-- | X is the number of Dark locations in play.
instance HasModifiersFor TwistingShadows where
  getModifiersFor (TwistingShadows a) = do
    x <- length <$> select (LocationWithTrait Dark)
    modified a a [ThreatModifier x, QuestPointModifier x]

-- | When Revealed: Remove all progress from each Dark location in play.
-- Shadow: Attacking enemy gets +1 attack for each Dark location in play.
instance RunMessage TwistingShadows where
  runMessage msg l@(TwistingShadows a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      dark <- select (LocationWithTrait Dark)
      for_ dark \lid -> do
        progress <- (.progress) <$> getLocation lid
        when (progress > 0) $ removeTokens a lid Progress progress
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      x <- length <$> select (LocationWithTrait Dark)
      enemyGetsAttack a eid x
      pure l
    _ -> TwistingShadows <$> runMessage msg a
