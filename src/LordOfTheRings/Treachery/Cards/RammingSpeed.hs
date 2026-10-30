module LordOfTheRings.Treachery.Cards.RammingSpeed (rammingSpeed) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RammingSpeed = RammingSpeed TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rammingSpeed :: TreacheryCard RammingSpeed
rammingSpeed = treachery RammingSpeed Cards.rammingSpeed

-- | When Revealed: If you are off-course, the engaged Ship enemy with the highest attack
-- makes an immediate attack. Until the end of the phase, add 1 to the total threat in the
-- staging area for each damage dealt by this attack. If no attack is made as a result of
-- this effect, Ramming Speed! gains doomed 2.
instance RunMessage RammingSpeed where
  runMessage msg t@(RammingSpeed attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      off <- isOffCourse
      ships <- traverse (\e -> (e,) <$> getEnemyAttack e) =<< select (shipEnemy <> EnemyEngagedWith AnyPlayer)
      let highest = maximumMay (map snd ships)
      engaged <- for [e | (e, n) <- ships, Just n == highest] \e -> (e,) . (.placement) <$> getEnemy e
      lead <- getLead
      if off && notNull engaged
        then chooseOrRunOneM lead do
          questionLabeled "rammingSpeed.choose"
          causedBy attrs
          for_ engaged \(e, placement) -> case placement of
            EngagedWith pid -> targeting e do
              attackModifier attrs e AttackDamageAddsStagingThreat
              immediateAttack pid e
            _ -> pure ()
        else raiseThreatFromDoomed 2
      pure t
    _ -> RammingSpeed <$> runMessage msg attrs
