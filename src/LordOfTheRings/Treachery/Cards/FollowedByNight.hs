module LordOfTheRings.Treachery.Cards.FollowedByNight (followedByNight) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FollowedByNight = FollowedByNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

followedByNight :: TreacheryCard FollowedByNight
followedByNight = treachery FollowedByNight Cards.followedByNight

-- | When Revealed: The first player (choose 1): deals 1 damage to all allies in play and
-- Followed by Night gains surge, or all enemies engaged with players make an immediate
-- attack, if able.
-- Shadow: Return attacking enemy to the staging area after it attacks.
instance RunMessage FollowedByNight where
  runMessage msg t@(FollowedByNight attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "followedByNight.choose"
        causedBy attrs
        labeled "followedByNight.damage" do
          allies <- select AnyAlly
          for_ allies \ally -> dealDamage attrs ally 1
          revealEncounterCard
        labeled "followedByNight.attack" $ eachPlayer \pid ->
          traverse_ (immediateAttack pid) =<< select (EnemyEngagedWith $ PlayerWithId pid)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> FollowedByNight <$> runMessage msg attrs
