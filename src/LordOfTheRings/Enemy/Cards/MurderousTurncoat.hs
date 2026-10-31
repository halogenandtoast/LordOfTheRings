module LordOfTheRings.Enemy.Cards.MurderousTurncoat (murderousTurncoat) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (placeUnderLocation)

newtype MurderousTurncoat = MurderousTurncoat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

murderousTurncoat :: EnemyCard MurderousTurncoat
murderousTurncoat = enemy MurderousTurncoat Cards.murderousTurncoat

-- | Forced: At the end of the combat phase, place Murderous Turncoat facedown underneath
-- the active location if you are not at stage 3.
instance HasAbilities MurderousTurncoat where
  getAbilities (MurderousTurncoat a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase]

-- | When Revealed: Murderous Turncoat makes an immediate attack against the first player.
instance RunMessage MurderousTurncoat where
  runMessage msg e@(MurderousTurncoat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      stage <- getMainStage
      active <- select IsActiveLocation
      when (stage /= 3) $ for_ (take 1 active) \lid ->
        placeUnderLocation lid (MkEncounterCard attrs.cardId (toCardCode attrs))
      pure e
    _ -> MurderousTurncoat <$> runMessage msg attrs
