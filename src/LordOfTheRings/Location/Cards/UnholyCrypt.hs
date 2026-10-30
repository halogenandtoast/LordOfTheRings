module LordOfTheRings.Location.Cards.UnholyCrypt (unholyCrypt) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateDestroyedBy, reanimatedDeadInPlay)

newtype UnholyCrypt = UnholyCrypt LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

unholyCrypt :: LocationCard UnholyCrypt
unholyCrypt = location UnholyCrypt Cards.unholyCrypt

-- | Unholy Crypt gets +1 threat for each Reanimated Dead in play.
instance HasModifiersFor UnholyCrypt where
  getModifiersFor (UnholyCrypt a) = do
    n <- length <$> reanimatedDeadInPlay
    when (n > 0) $ modifySelf a [ThreatModifier n]

-- | Shadow: If this attack destroys a character, reanimate that character and deal it 1
-- shadow card.
instance RunMessage UnholyCrypt where
  runMessage msg l@(UnholyCrypt a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      reanimateDestroyedBy eid Nothing
      pure l
    _ -> UnholyCrypt <$> runMessage msg a
