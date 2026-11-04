module LordOfTheRings.Treachery.Cards.RodeLikeAGale (rodeLikeAGale) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RodeLikeAGale = RodeLikeAGale TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rodeLikeAGale :: TreacheryCard RodeLikeAGale
rodeLikeAGale = treachery RodeLikeAGale Cards.rodeLikeAGale

-- | When Revealed: Return each engaged Nazgûl enemy to the staging area. If no enemies were
-- returned to the staging area by this effect, Rode Like a Gale gains surge.
-- Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage RodeLikeAGale where
  runMessage msg t@(RodeLikeAGale attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      engaged <- select (EnemyWithTrait Nazgul <> EnemyEngagedWith AnyPlayer)
      for_ engaged \e -> placeEnemy e StagingArea
      when (null engaged) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> RodeLikeAGale <$> runMessage msg attrs
