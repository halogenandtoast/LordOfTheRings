module LordOfTheRings.Treachery.Cards.OrcishHowls (orcishHowls) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OrcishHowls = OrcishHowls TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcishHowls :: TreacheryCard OrcishHowls
orcishHowls = treachery OrcishHowls Cards.orcishHowls

{- | When Revealed: Deal 1 damage to each enemy in the staging area. Until the end of the
phase, each enemy gets +1 threat for each damage on it. If there are no enemies in the
staging area, Orcish Howls gains surge.
Shadow: If you are engaged with another enemy, resolve this attack against that enemy.
-}
instance RunMessage OrcishHowls where
  runMessage msg t@(OrcishHowls attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select EnemyInStagingArea
      for_ enemies \e -> dealDamage attrs e 1
      phaseModifier attrs GameTarget EnemiesGetThreatPerDamage
      doStep 1 msg
      pure t
    -- counted once the damage is dealt
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      none <- selectNone EnemyInStagingArea
      when none revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      others <- select (EnemyEngagedWith (PlayerWithId pid) <> not_ (EnemyWithId eid))
      chooseOrRunOneM pid do
        questionLabeled "orcishHowls.redirect"
        causedBy (CardIdTarget cid)
        targets others (redirectAttack eid)
      pure t
    _ -> OrcishHowls <$> runMessage msg attrs
