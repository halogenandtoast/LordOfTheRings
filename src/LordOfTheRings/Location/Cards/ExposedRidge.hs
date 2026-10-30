module LordOfTheRings.Location.Cards.ExposedRidge (exposedRidge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype ExposedRidge = ExposedRidge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

exposedRidge :: LocationCard ExposedRidge
exposedRidge = location ExposedRidge Cards.exposedRidge

-- | While Exposed Ridge is the active location, it gains: "Forced: At the end of the
-- refresh phase, each player deals 1 damage to a character he controls."
instance HasAbilities ExposedRidge where
  getAbilities (ExposedRidge a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.placement == ActiveLocation]

-- | Shadow: Attacking enemy gets +1 attack for each damaged character the defending player
-- controls.
instance RunMessage ExposedRidge where
  runMessage msg l@(ExposedRidge a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> dealDamageToOneCharacter a p 1
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      damaged <- damagedCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      enemyGetsAttack (CardIdSource cid) eid (length damaged)
      pure l
    _ -> ExposedRidge <$> runMessage msg a
