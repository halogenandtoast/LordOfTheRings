module LordOfTheRings.Treachery.Cards.GetThatDwarf (getThatDwarf) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (attackNalir, getNalir)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GetThatDwarf = GetThatDwarf TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

getThatDwarf :: TreacheryCard GetThatDwarf
getThatDwarf = treachery GetThatDwarf Cards.getThatDwarf

-- | When Revealed: The enemy in the staging area with the highest attack engages the first
-- player and makes an immediate attack against Nalir. If there are no enemies in the staging
-- area, Get That Dwarf! gains Surge. The first player picks among those tied.
-- Shadow: If this attack is undefended, deal 2 damage to Nalir.
instance RunMessage GetThatDwarf where
  runMessage msg t@(GetThatDwarf attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      staged <- traverse (\e -> (e,) <$> getEnemyAttack e) =<< select EnemyInStagingArea
      let highest = foldr (max . snd) 0 staged
      lead <- getLead
      if null staged
        then revealEncounterCard
        else chooseOrRunOneM lead do
          questionLabeled "getThatDwarf.choose"
          causedBy attrs
          targets [e | (e, n) <- staged, n == highest] \e -> do
            engageEnemy lead e
            attackNalir attrs e
      pure t
    ResolveShadowEffect cid _ _ [] | cid == attrs.cardId -> do
      getNalir >>= traverse_ \n -> dealDamage attrs n 2
      pure t
    _ -> GetThatDwarf <$> runMessage msg attrs
