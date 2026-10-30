module LordOfTheRings.Treachery.Cards.CaptivesOfGornakh (captivesOfGornakh) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaptivesOfGornakh = CaptivesOfGornakh TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

captivesOfGornakh :: TreacheryCard CaptivesOfGornakh
captivesOfGornakh = treachery CaptivesOfGornakh Cards.captivesOfGornakh

-- | When Revealed: Each player chooses one of their allies and captures it underneath the
-- encounter card with the highest threat. (The enemies and locations in play; with several
-- tied, the player chooses.)
-- Shadow: If this attack damages a character, capture that character underneath the
-- attacking enemy instead of dealing damage.
instance RunMessage CaptivesOfGornakh where
  runMessage msg t@(CaptivesOfGornakh attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse (\e -> (toTarget e,) <$> getEnemyThreat e) =<< select AnyEnemy
      locations <- traverse (\l -> (toTarget l,) <$> getLocationThreat l) =<< select AnyLocation
      let threats = enemies <> locations
          highest = maximumMay (map snd threats)
          hosts = [h | (h, n) <- threats, Just n == highest]
      eachPlayer \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        unless (null hosts) $ chooseOrRunOneM pid do
          questionLabeled "captivesOfGornakh.ally"
          causedBy attrs
          targets allies \ally -> do
            card <- toCard <$> getAlly ally
            chooseOrRunOneM pid do
              questionLabeled "captivesOfGornakh.host"
              causedBy attrs
              targets hosts \h -> placeUnderneath h [card]
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      attackModifier attrs eid CaptureInsteadOfAttackDamage
      pure t
    _ -> CaptivesOfGornakh <$> runMessage msg attrs
