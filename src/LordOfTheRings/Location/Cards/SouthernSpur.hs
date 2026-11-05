module LordOfTheRings.Location.Cards.SouthernSpur (southernSpur) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype SouthernSpur = SouthernSpur LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southernSpur :: LocationCard SouthernSpur
southernSpur = location SouthernSpur Cards.southernSpur

instance HasAbilities SouthernSpur where
  getAbilities (SouthernSpur a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase]

instance RunMessage SouthernSpur where
  runMessage msg l@(SouthernSpur a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        mods <- getModifiers (PlayerTarget pid)
        when (notNull [() | PlayedCardThisPhase _ AllyType <- mods]) $ encounterRaisesThreat pid 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 2
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      eachPlayer \pid -> encounterRaisesThreat pid 3
      pure l
    _ -> SouthernSpur <$> runMessage msg a
